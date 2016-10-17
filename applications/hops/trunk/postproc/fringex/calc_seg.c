/************************************************************************/
/*                                                                      */
/* After the totals have been accumulated for each segment (in routine  */
/* accum_segs() ), the geometrical calculations must be performed to    */
/* derive the phases and many other quantities for each segment.  This  */
/* routine performs that function.  It's a little complicated, and has  */
/* therefore been heavily commented.                                    */
/*                                                                      */
/*      Inputs:         fxp             all filled in                   */
/*                      seg             segment number (array index)    */
/*                                                                      */
/*      Output:         fxp             adata element filled in         */
/*                                                                      */
/* Created October 11 1995 by CJL                                       */
/* Modified for mk4 file system           2007.10.3   rjc               */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "fringex.h"

int calc_seg (struct fxparam *fxp, int seg)
    {
    int offset, time_tag, scan_time;
    double freq_ratio, avsec, epoch, epochoff,
           amp, tamp, pdum, tphase, rphase, tephase, bl_phase;
    struct mk4_fringe *fr;

                                        /* Convenience pointers */
    fr = fxp->fringe;

    if (fxp->seglen[seg] <= 0.0)
        return (1);

    /************************* SET TIMES *****************************/
                                        /* Scan time set to start of segment */
                                        /* adata.time_tag preset in fill_aline() */
                                        /* Note: time_tag treated differently for */
                                        /* different cases of omode */
    scan_time = fxp->adata.time_tag - fxp->adata.scan_offset;
    if (fxp->nsecs == 9999) 
        time_tag = fxp->adata.time_tag;
    else
        {
        if (fxp->mode & OMODE) 
            time_tag = (fxp->segstart + 0.5 * (double)(seg-1))  * fxp->nsecs;
        else 
            time_tag = (fxp->segstart + seg + 0.5) * fxp->nsecs;
        }
                                        /* offset to nearest sec */
    offset = fxp->reftime + (int)(fxp->segsec[seg] / fxp->segcount[seg] + 0.5) 
                                                                        - time_tag;
                                        /* fictitious time in mode q */
    if (fxp->mode & QMODE) 
        time_tag = fxp->tstart + seg;
                                        /* epoch is for calculations */
                                        /* Force corel epoch for nsec=9999 */
                                        /* epochoff is diff between segment epoch */
                                        /* and scan reference time */
    avsec = fxp->reftime + fxp->segsec[seg] / fxp->segcount[seg];

    if (fxp->nsecs == 9999) 
        epoch = fxp->reftime;
    else
        {
        if (fxp->mode & OMODE) 
            {
            if (seg%2 == 1)
                epoch = (floor(avsec/fxp->nsecs) + 0.5) * fxp->nsecs;
            else
                epoch = floor(avsec/fxp->nsecs + 0.5) * fxp->nsecs;
            }
        else 
            epoch = (floor(avsec/fxp->nsecs) + 0.5) * fxp->nsecs;
        }
    epochoff = epoch - fxp->reftime;

    /************************* CALCULATE PHASES *****************************/
                                        /* Segment amplitude and phase */
    amp = sqrt (fxp->rsum[seg]*fxp->rsum[seg] + 
                        fxp->isum[seg]*fxp->isum[seg]) / fxp->seglen[seg];
    
    if ((fxp->isum[seg] != 0) && (fxp->rsum[seg] != 0))
        tphase = atan2 (fxp->isum[seg], fxp->rsum[seg]) * 180.0 / M_PI;
    else 
        tphase = 0;
    
    msg ("segment amp & phase = %g %g deg", -1, amp, tphase);
                                        /* Adjust user offsets to new freq and epoch */
    freq_ratio = fxp->reffreq / fxp->ffit_reffreq;
                                        // since there are no derivs wrt source position
                                        // available, don't make any corrections now
    // tphase += fr->t4500.fr_asec_ew * fxp->raoff * freq_ratio * 360.0*1e-3;
    // tphase += fr->t4500.fr_asec_ns * fxp->decoff * freq_ratio * 360.0*1e-3;
    // tphase += fr->t4500.ur_deriv * fxp->raoff * epochoff * freq_ratio * 360.0*1e-6;
    // tphase += fr->t4500.vr_deriv * fxp->decoff * epochoff * freq_ratio * 360.0*1e-6;

                                        /* Phase residual to corel+fourfit */
    rphase = tphase;
                                        // add back in the fourfit model fit
    tphase += epochoff * fxp->rateoff * fxp->reffreq * 360.0;
    tphase += fxp->delayoff * (fxp->reffreq - fxp->ffit_reffreq) * 360.0;

                                        // calculate difference ofmodel phase 
                                        // for two stations in baseline
                                        // --note that mk4 does all of its
                                        // modelling wrt an earth-centered epoch system
    model (epoch, fxp, &bl_phase);
    tephase = tphase + bl_phase;
                                        /* Convert all phases to 0-360 */
    rphase = modf (rphase/360.0, &pdum) * 360.0;
    if (rphase < 0) 
        rphase += 360.0;

    tephase = modf (tephase/360.0, &pdum) * 360.0;
    if (tephase < 0) 
        tephase += 360.0;

    /************************* FILL IN AFILE LINE *****************************/
                                        /* Now start filling afile structure */
                                        /* phase residual to corel+fourfit in field 17 */
    fxp->adata.resid_phas = rphase;
    fxp->adata.datatype[1]='f';
                                        /* Total earth-centered always present */
    fxp->adata.total_phas = tephase;

    if (fxp->nsecs < fxp->acc_period) 
        fxp->adata.datatype[0] = 'A';
    else if (fxp->mode & OMODE) 
        fxp->adata.datatype[0] = 'O';
    else 
        fxp->adata.datatype[0] = 'C';

    fxp->adata.time_tag = time_tag;
    fxp->adata.scan_offset = time_tag - scan_time;
                                        /* Minutes/seconds of calculation epoch */
    fxp->adata.epoch[0] = 0;		// was: smin, but relative to what hour?
    fxp->adata.epoch[1] = 0;		// was: ssec, but relative to what hour?
    fxp->adata.offset = offset;
    fxp->adata.ref_freq = fxp->reffreq;
    fxp->adata.amp = amp * 10000.0;
                                        /* Amplitude needs to have correction */
                                        /* for rate and sbd removed, in order to */
                                        /* do correct incoherent average debiasing */
                                        /* in average */
    if (fxp->no_amp_corr)
        {
        if ((fxp->amp_corr_fact > 1.6) || (fxp->amp_corr_fact < 0.9))
            {
            msg ("Attempted to remove amplitude correction with invalid factor", 2);
            msg ("Probable cause is outdated version of fourfit used", 2);
            return (-1);
            }
        fxp->adata.amp /= fxp->amp_corr_fact;
        }
                                        /* convert from fringes/asec/ghz */
                                        /* and millhz/asec/ghz to megalambda */
                                        /* adjust for epoch */
    // unfortunately, there is no u & v data available with mk4
    // fxp->adata.u = (fxp->reffreq * 648.0) * (rb->t2600.u_obsfreq  / (pi*1e06)
    //                 +rb->t2600.u_obsfreq_rate *  epochoff / (pi*1e09));
    // fxp->adata.v = (fxp->reffreq * 648.0) * (rb->t2600.v_obsfreq  / (pi*1e06)
    //                 +rb->t2600.v_obsfreq_rate *  epochoff / (pi*1e09));
    // fxp->adata.u = 0.0;
    // fxp->adata.v = 0.0;

    /* fxp->adata.u and fxp->adata.v were loaded from the fringe file */

    if (fxp->nsecs != 9999) 
        fxp->adata.duration = (int)fxp->nsecs;

    fxp->adata.snr = fr->t208->snr * sqrt (fxp->seglen[seg] / fxp->numaccp);

    tamp = fr->t208->amplitude;
    fxp->adata.snr *= amp / tamp;
    msg ("%g %g %g %g", -1, fr->t208->snr, fxp->adata.snr, amp, tamp);
    
    fxp->adata.length = floor((fxp->seglen[seg]/fxp->numaccp) * fr->t206->intg_time + 0.5);
    fxp->adata.mbdelay = fxp->delayoff;
    fxp->adata.delay_rate = fxp->rateoff * 1e06;

    return (0);

    }

/*
 * eof
 */
