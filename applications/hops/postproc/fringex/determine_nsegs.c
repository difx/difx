/************************************************************************/
/*                                                                      */
/* After the type 2 file has been read in, the number of segments needs */
/* to be determined. This function is essentially a dummy version of    */
/* accum_segs.c used just to calculate the number of segments needed,   */
/* so they can be (re)allocated before use.                             */
/*                                                                      */
/*      Inputs:         fxp             pointer to fringe file already  */
/*                                      filled in                       */
/*                                                                      */
/*      Output:         nsegs            max number of segments         */
/*                                                                      */
/* Created January 22 2018 JPB                                          */
/*                                                                      */
/************************************************************************/

#include <math.h>
#include "fringex.h"

int determine_nsegs (const struct fxparam *fxp)
    {
    int i, ap, segnum, ch, pass, npass;
    double timeoff, aptime, freq, earliest;
    double tstart, fxptstart, fxpsegstart;
    int fxpnsegs = 0;
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
    fxptstart = fxp->adata.time_tag + fxp->adata.scan_offset + 0.5 * fxp->acc_period;

    fxpsegstart = floor (fxptstart/fxp->nsecs);

                                        /* Loop over all type_212 records */
                                        // there is one t212 per (possibly dsb) freq channel
    for (i=0; i<fr->n212; i++)
        {
        rec = fr->t212[i];              // convenience pointer to specific t212 record

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
                        segnum = npass * ((aptime/fxp->nsecs) - fxpsegstart);
                    else 
                        segnum = npass * ((aptime/fxp->nsecs) + 0.5 - fxpsegstart);
                    }
                else 
                    segnum = 0;

                if (segnum < 0 )
                    segnum = 0;
                                        /* Maximum segment number + 1 = no. of segs */
                if (segnum >= fxpnsegs)
                    {
                    fxpnsegs = segnum + 1;
                    }

                }                       /* End pass loop */

            }                           /* End ap loop */
        }                               /* End type-212 record loop */

        return fxpnsegs;
    }

/*
 * eof
 */
