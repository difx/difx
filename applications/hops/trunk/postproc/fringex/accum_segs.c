/************************************************************************/
/*                                                                      */
/* After the type 2 file has been read in, the segment boundaries need  */
/* to be defined, and all the accumulation periods contributing to      */
/* each segment must be added together coherently.  Various other       */
/* statistics also need to be accumulated.  Once this is done, the      */
/* accumulated sums for each segment can be passed on to the calc_seg() */
/* routine, which computes the values to go into the output record      */
/* for a given segment.                                                 */
/*                                                                      */
/*      Inputs:         fxp             pointer to fringe file already  */
/*                                      filled in                       */
/*                                                                      */
/*      Output:         fxp             segment arrays filled in        */
/*                                                                      */
/* Created October 12 1995 by CJL                                       */
/* Modified to work with Mk4 files                2007.10.2   rjc       */
/*                                                                      */
/************************************************************************/
#include <math.h>
#include "fringex.h"

void accum_segs (struct fxparam *fxp)
    {
    int i, ap, segnum, ch, pass, npass;
    double tph1, tph2, tph3, tph4, tph5;
    double timeoff, aptime, freq, earliest;
    double weight, amp, phase, phase_adj, real, imag;
    double tstart;
    struct mk4_fringe *fr;
    struct type_212 *rec;
                                        /* Convenience pointer */
    fr = fxp->fringe;
                                        /* -o mode, see comment below */
    if (fxp->mode & OMODE)
        npass = 2;
    else 
        npass = 1;
                                        /* Init tstart on earliest ap */
                                        /* segstart relative to Jan 1 1980 */
                                        /* This lines up segments globally */
    fxp->tstart = fxp->adata.time_tag + fxp->adata.scan_offset + 0.5 * fxp->acc_period;

    fxp->segstart = floor (fxp->tstart/fxp->nsecs);

                                        /* Loop over all type_212 records */
                                        // there is one t212 per (possibly dsb) freq channel
    for (i=0; i<fr->n212; i++)
        {
        rec = fr->t212[i];              // convenience pointer to specific t212 record

                                        /* get LO frequency */
        fxp->fchan[i] = fr->t203->channels[fr->t205->ffit_chan[rec->channel].channels[0]].ref_freq;
                                        // convert freq's to MHz
        fxp->fchan[i] *= 1e-6;

        freq = fxp->fchan[i];
                                        /* Calculate phase offsets corresponding */
                                        /* to user-specified offsets in rate/delay */
                                        /* or ra/dec.  Also do pcal rate */
        tph1 = fxp->dprate * (freq / fxp->ffit_reffreq) * 2.0 * PI;

                                        // since there is currently (2007.10.3) no information
                                        // available about phase as a fn. of position, set to 0
        tph2 = 0.0;
        tph3 = 0.0;
        // tph2 = ((fr->t4500.fr_asec_ew * fxp->raoff) + (fr->t4500.fr_asec_ns * fxp->decoff))
        //                * (freq / fxp->ffit_reffreq) * 2.0e-3 * PI;
        // tph3 = ((fr->t4500.ur_deriv * fxp->raoff) + (fr->t4500.vr_deriv * fxp->decoff))
        //                * (freq / fxp->ffit_reffreq) * 2.0e-6 * PI;

        tph4 = 2.0 * PI * freq * (fxp->rateoff - fr->t208->resid_rate);
        tph5 = 2.0 * PI * (freq - fxp->ffit_reffreq) * (fxp->delayoff - fr->t208->resid_mbd);

                                        /* Loop over AP's in this type 212 */
        for (ap=0; ap<rec->nap; ap++)
            {
                                        /* Skip empty aps */
            if (rec->data[ap].amp < 0)
                continue;
                                        /* Calculate time of middle of a.p. */
                                        // adjust timeoff for first ap number rjc 2010.2.26
                                        // further adjust timeoff so that 0 point reference
                                        // is the fourfit reference time (usually scan center)
                                        // rjc  2010.5.21
            tstart = time_to_int ((int)fr->t206->start.year, 
                                  (int)fr->t206->start.day, 
                                  (int)fr->t206->start.hour,
                                  (int)fr->t206->start.minute,
                                  (int)fr->t206->start.second);
            timeoff = (fr->t206->first_ap + ap + 0.5) * fxp->acc_period
                      + (tstart - fxp->reftime);
            aptime = fxp->reftime + timeoff;
                                        /* Extract weighted amplitude, and phase */
            weight = rec->data[ap].weight;
                                        // weight of ssb channel only half that of a dsb channel
            if (fr->t205->ffit_chan[rec->channel].channels[1] < 0) 
                weight /= 2;
                
            amp = rec->data[ap].amp * weight / 10000.0;
            phase = rec->data[ap].phase;
                                        /* Calculate phase adjustment */
            phase_adj = timeoff * (tph1 + tph3 + tph4) + tph2 + tph5;
                                        /* Compute vector for this ap */
            real = amp * cos((double)(phase - phase_adj));
            imag = amp * sin((double)(phase - phase_adj));
                                        /* Here's some tricky stuff.  In -o */
                                        /* mode, use 2x as many segments, */
                                        /* interleaving natural and 1/2-segment */
                                        /* offset segments.  The segment number */
                                        /* is calculated differently, and there */
                                        /* are 2 passes through next code section */
                                        /* instead of just 1.  npass set at top */
                                        /* of routine */
            for (pass=0; pass<npass; pass++)
                {
                                        /* Which segment is this (segnum)? */
                                        /* segnum is relative to segstart */
                if (fxp->nsecs != 9999) 
                    {
                    if (pass == 0) 
                        segnum = npass * ((aptime/fxp->nsecs) - fxp->segstart);
                    else 
                        segnum = npass * ((aptime/fxp->nsecs) + 0.5 - fxp->segstart);
                    }
                else 
                    segnum = 0;

                if (segnum < 0 || segnum >= MAXSEG) 
                    segnum = 0;
                                        /* Maximum segment number + 1 = no. of segs */
                if (segnum >= fxp->nsegs) 
                    fxp->nsegs = segnum + 1;

                                        /* Add vectorially to this segment */
                                        /* and keep statistics */
                fxp->rsum[segnum] += real;
                fxp->isum[segnum] += imag;
                fxp->segsec[segnum] += timeoff;
                fxp->segcount[segnum]++;
                fxp->seglen[segnum] += weight;
                }                       /* End pass loop */
            fxp->numaccp += weight;
            }                           /* End ap loop */
        }                               /* End type-212 record loop */
    }

/*
 * eof
 */
