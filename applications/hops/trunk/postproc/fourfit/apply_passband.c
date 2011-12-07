/*  apply passband to cross power spectrum, if specified
 *                                    rjc 2002.4.19  */

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <stdio.h>

void apply_passband (int sb,
                     struct freq_corel *fdata,
                     complex *xp_spectrum,
                     int npts)
    {
    int i,
        ibot,
        itop,
        incband;

    double bw,
           bottom,
           top,
           param_passband_0_, param_passband_1_;
    
    extern struct type_param param;
    extern struct type_status status;
    complex c_zero (void);
    
                                    /* return immediately if no filtering
                                     * is desired (still set to defaults) */
    if (param.passband[0] == 0.0 && param.passband[1] == 1.0E6)
        return;

    if (param.passband[0] < param.passband[1])
        {
        incband = 1;
        param_passband_0_ = param.passband[0];
        param_passband_1_ = param.passband[1];
        }
    else
        {
        incband = 0;
        param_passband_0_ = param.passband[1];
        param_passband_1_ = param.passband[0];
        }

    bw = 0.5e-6 / param.samp_period;/* in MHz, assumes Nyquist sampling */

                                    /* determine top and bottom frequency of
                                     * this current sideband (MHz) */
    if (sb)
        {                           /* LSB */
        top = fdata->frequency;
        bottom = top - bw;

        if (param_passband_1_ < bottom)
            ibot = npts + 1;
        else if (param_passband_1_ < top)
            ibot = (1.0 - (param_passband_1_ - bottom) / bw) * npts / 2 + 0.5;
        else
            ibot = -1;
    
        if (param_passband_0_ < bottom)
            itop = npts + 1;
        else if (param_passband_0_ < top)
            itop = (1.0 - (param_passband_0_ - bottom) / bw) * npts / 2 + 0.5;
        else
            itop = -1;
        }
    else                            /* USB */
        {
        bottom = fdata->frequency;
        top = bottom + bw;

        if (param_passband_0_ < bottom)
            ibot = -1;
        else if (param_passband_0_ < top)
            ibot = (param_passband_0_ - bottom) / bw * npts / 2 + 0.5;
        else
            ibot = npts + 1;
    
        if (param_passband_1_ < bottom)
            itop = -1;
        else if (param_passband_1_ < top)
            itop = (param_passband_1_ - bottom) / bw * npts / 2 + 0.5;
        else
            itop = npts + 1;
        }
        

                                    /* zero out data outside of passband */
    for (i=0;  incband && i<npts; i++)
        if (i < ibot || i > itop)
            xp_spectrum[i] = c_zero ();
                                    /* OR: zero out data inside passband */
    for (i=0; !incband && i<npts; i++)
        if (i > ibot && i < itop)
            xp_spectrum[i] = c_zero ();
    msg ("bw %lf bottom %lf top %lf ibot %d itop %d npts %d %s", 0,
          bw, bottom, top, ibot, itop, npts,
          incband ? "include" : "exclude");
    }
