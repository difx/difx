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
        itop;

    double bw,
           bottom,
           top;
    
    extern struct type_param param;
    extern struct type_status status;
    complex c_zero (void);
    
                                    /* return immediately if no filtering
                                     * is desired (still set to defaults) */
    if (param.passband[0] == 0.0 && param.passband[1] == 1.0E6)
        return;


    bw = 0.5e-6 / param.samp_period;/* in MHz, assumes Nyquist sampling */

                                    /* determine top and bottom frequency of
                                     * this current sideband (MHz) */
    if (sb)
        {                           /* LSB */
        top = fdata->frequency;
        bottom = top - bw;

        if (param.passband[1] < bottom)
            ibot = npts + 1;
        else if (param.passband[1] < top)
            ibot = (1.0 - (param.passband[1] - bottom) / bw) * npts / 2 + 0.5;
        else
            ibot = -1;
    
        if (param.passband[0] < bottom)
            itop = npts + 1;
        else if (param.passband[0] < top)
            itop = (1.0 - (param.passband[0] - bottom) / bw) * npts / 2 + 0.5;
        else
            itop = -1;
        }
    else                            /* USB */
        {
        bottom = fdata->frequency;
        top = bottom + bw;

        if (param.passband[0] < bottom)
            ibot = -1;
        else if (param.passband[0] < top)
            ibot = (param.passband[0] - bottom) / bw * npts / 2 + 0.5;
        else
            ibot = npts + 1;
    
        if (param.passband[1] < bottom)
            itop = -1;
        else if (param.passband[1] < top)
            itop = (param.passband[1] - bottom) / bw * npts / 2 + 0.5;
        else
            itop = npts + 1;
        }
        

                                    /* zero out data outside of passband */
    for (i=0; i<npts; i++)
        if (i < ibot || i > itop)
            xp_spectrum[i] = c_zero ();
    msg ("bw %lf bottom %lf top %lf ibot %d itop %d npts %d", 0,
          bw, bottom, top, ibot, itop, npts);
    }
