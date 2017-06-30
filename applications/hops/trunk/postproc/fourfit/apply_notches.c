/*
 * apply notches to the cross power spectrum, if specified
 *
 * for clarity of code, lsb and usb are separated into two routines
 * this is all similar to apply_passband, except there is only
 * exclusion, not inclusion.
 */

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <stdio.h>

extern void   msg (char *, int, ...);
extern struct type_param param;

void apply_lsb_notch(double lo, double hi,
                     
                     struct freq_corel *fdata,
                     complex *xp_spectrum,
                     int npts,
                     struct data_corel *datum)
    {
    int ii, ibot, itop, nuked;
    double bw, bottom, top;
    float nibble, origfrac = datum->lsbfrac;

    bw = 0.5e-6 / param.samp_period;
    top = fdata->frequency;
    bottom = top - bw;

    if (hi < bottom)
        ibot = npts + 1;
    else if (hi < top)
        ibot = (1.0 - (hi - bottom) / bw) * npts / 4 + 0.5;
    else
        ibot = -1;

    if (lo < bottom)
        itop = npts + 1;
    else if (lo < top)
        itop = (1.0 - (lo - bottom) / bw) * npts / 4 + 0.5;
    else
        itop = -1;

    for (ii = 0, nuked = 0; ii < npts; ii++)
        if (ii > ibot && ii < itop)
            {
            xp_spectrum[ii] = 0.0; /* zero out the band */
            nuked ++;
            }

    nibble = (nuked > 0) ? (double)nuked / (double)(npts / 4) : 0.0;
    if (nibble > 0.0 && nibble < datum->lsbfrac) datum->lsbfrac -= nibble;
    msg ("notch [%lf - %lf]: nibbed %lf by %lf down to %lf",
        1, lo, hi, origfrac, nibble, datum->lsbfrac);
    }

void apply_usb_notch(double lo, double hi,
                     struct freq_corel *fdata,
                     complex *xp_spectrum,
                     int npts,
                     struct data_corel *datum)
    {
    int ii, ibot, itop, nuked;
    double bw, bottom, top, n_lo, n_hi;
    float nibble, origfrac = datum->usbfrac;

    bw = 0.5e-6 / param.samp_period;
    bottom = fdata->frequency;
    top = bottom + bw;

    if (lo < bottom)
        ibot = -1;
    else if (lo < top)
        ibot = (lo - bottom) / bw * npts / 4 + 0.5;
    else
        ibot = npts + 1;

    if (hi < bottom)
        itop = -1;
    else if (hi < top)
        itop = (hi - bottom) / bw * npts / 4 + 0.5;
    else
        itop = npts + 1;

    for (ii = 0, nuked = 0; ii < npts; ii++)
        if (ii > ibot && ii < itop)
            {
            xp_spectrum[ii] = 0.0; /* zero out the band */
            nuked ++;
            }

    nibble = (nuked > 0) ? (double)nuked / (double)(npts / 4) : 0.0;
    if (nibble > 0.0 && nibble < datum->usbfrac) datum->usbfrac -= nibble;
    msg ("notch [%lf - %lf]: nibbed %lf by %lf down to %lf",
        1, lo, hi, origfrac, nibble, datum->usbfrac);
    }


void apply_notches(int sb, int ap,
                   struct freq_corel *fdata,
                   complex *xp_spectrum,
                   int npts,
                   struct data_corel *datum)
    {
    int ii;
    double lo, hi;

    if (param.nnotches) msg ("Ap %d: applying %d notches to channel %lf",
        1,ap,param.nnotches,fdata->frequency);
    for (ii = 0; ii < param.nnotches; ii++)
        {
        lo = param.notches[ii][0];
        hi = param.notches[ii][1];
        if (hi <= lo)
            {
            msg("Ignoring illegal notch(%d): %g <= %g",2,ii,hi,lo);
            continue;
            }
        if (sb) apply_lsb_notch(lo, hi, fdata, xp_spectrum, npts, datum);
        else    apply_usb_notch(lo, hi, fdata, xp_spectrum, npts, datum);
        }
    }

/* eof */
