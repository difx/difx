/*
 * apply passband to cross power spectrum, if specified
 *                                    rjc 2002.4.19
 *
 * None of this code is called if datum->?sbfrac < 0
 *
 * The main confusion here is that at this point npts=2*nlags,
 * and the LSB and USB data lie entirely within npts/4 or nlags/2
 * based on param->corr_type = DIFX or param->corr_type = MK4HDW
 */

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <stdio.h>

extern void   msg (char *, int, ...);

void apply_passband (int sb, int ap,
                     struct freq_corel *fdata,
                     complex *xp_spectrum,
                     int npts,
                     struct data_corel *datum)
    {
    int i,
        ibot,
        itop,
        incband,
        nuked, dv;

    double bw,
           bottom,
           top,
           param_passband_0_, param_passband_1_;
    float *usbfracptr, usbreduce, usboldfrac;
    float *lsbfracptr, lsbreduce, lsboldfrac;

    extern struct type_param param;
    // extern struct type_sumb_tatus status;

    dv = (param.corr_type == DIFX) ? 4 : 2 ;
    
                                    /* return immediately if no filtering
                                     * is desired (still set to defaults) */
    if (param.passband[0] == 0.0 && param.passband[1] == 1.0E6)
        return;
                                    /* return if 'band' width is zero */
    if (param.passband[0] == param.passband[1])
        return;

    if (param.passband[0] < param.passband[1])
        {
        incband = 1;                /* this is an inclusion band */
        param_passband_0_ = param.passband[0];
        param_passband_1_ = param.passband[1];
        }
    else
        {
        incband = 0;                /* this is an exclusion band */
        param_passband_0_ = param.passband[1];
        param_passband_1_ = param.passband[0];
        }
    /* assert( param_passband_0_ < param_passband_1_ ); */

    bw = 0.5e-6 / param.samp_period;/* in MHz, assumes Nyquist sampling */

                                    /* determine top and bottom frequency of
                                     * this current sideband (MHz) */
    if (sb)
        {                           /* LSB:  3*npts/4 .. npts-1 */
        top = fdata->frequency;
        bottom = top - bw;

        if (param_passband_1_ < bottom)
            ibot = npts + 1;
        else if (param_passband_1_ < top)
            ibot = (1.0 - (param_passband_1_ - bottom) / bw) * npts /dv + 0.5;
        else
            ibot = -1;
    
        if (param_passband_0_ < bottom)
            itop = npts + 1;
        else if (param_passband_0_ < top)
            itop = (1.0 - (param_passband_0_ - bottom) / bw) * npts /dv + 0.5;
        else
            itop = -1;

        lsbfracptr = &datum->lsbfrac;
        usbfracptr = 0;             /* disable accounting logic below */
        }
    else                            /* USB:  0 .. npts/4-1 */
        {
        bottom = fdata->frequency;
        top = bottom + bw;

        if (param_passband_0_ < bottom)
            ibot = -1;
        else if (param_passband_0_ < top)
            ibot = (param_passband_0_ - bottom) / bw * npts /dv + 0.5;
        else
            ibot = npts + 1;
    
        if (param_passband_1_ < bottom)
            itop = -1;
        else if (param_passband_1_ < top)
            itop = (param_passband_1_ - bottom) / bw * npts /dv + 0.5;
        else
            itop = npts + 1;

        usbfracptr = &datum->usbfrac;
        lsbfracptr = 0;             /* disable accounting logic below */
        }
    /* assert ( ibot < itop ); */

                                    /* zero out data outside of passband */
    nuked = 0;
    for (i=0;  incband && i<npts/dv; i++)
        if (i < ibot || i > itop)
            {
            xp_spectrum[i] = 0.0;
            nuked ++;
            }
                                    /* OR: zero out data inside passband */
    for (i=0; !incband && i<npts/dv; i++)
        if (i > ibot && i < itop)
            {
            xp_spectrum[i] = 0.0;
            nuked ++;
            }

    /* implicitly assuming USB and LSB bands are same size: npts/dv */

    usbreduce = lsbreduce = 0.0;
    if (usbfracptr)
        {
        usboldfrac = *usbfracptr;
        usbreduce = (nuked >= npts/dv)
                  ? 0.0 : (double)(npts/dv)/(double)(npts/dv - nuked);
        *usbfracptr *= usbreduce;
        msg ("usb: ap %d bw %.3lf bot %.3lf top %.3lf ibot %3d itop %3d "
             "nuke/npts %3d/%3d %s %.3lf->%.3lf (%.3lf) %.2f %.2f",
            1, ap, bw, bottom, top, ibot, itop, nuked, npts,
            incband ? "include" : "exclude",
            usboldfrac, *usbfracptr, usbreduce,
            param_passband_0_, param_passband_1_);
        }

    if (lsbfracptr)
        {
        lsboldfrac = *lsbfracptr;
        lsbreduce = (nuked >= npts/dv)
                  ? 0.0 : (double)(npts/dv)/(double)(npts/dv - nuked);
        *lsbfracptr *= lsbreduce;
        msg ("lsb: ap %d bw %.3lf bot %.3lf top %.3lf ibot %3d itop %3d "
             "nuke/npts %3d/%3d %s %.3lf->%.3lf (%.3lf) %.2f %.2f",
            1, ap, bw, bottom, top, ibot, itop, nuked, npts,
            incband ? "include" : "exclude",
            lsboldfrac, *lsbfracptr, lsbreduce,
            param_passband_0_, param_passband_1_);
        }

    }

/* eof */
