/************************************************************************
* calc_normalization calculates the normalization quantities that       *
*   allow raw correlator counts to be converted into true correlations. *
*   If desired, the code will employ the sampler statistics; otherwise  *
*   nominal values appropriate to the correlation mode are used.        *
*                                                                       *
*   Inputs:    sb          sideband (0,1) = (upper, lower)              *
*              pol         polarizations (0,1,2,3) = (LL,RR,LR,RL)      *
*              datum       ptr to this AP's data_corel struct           *
*              pass        ptr to pass structure for these data         *
*   Outputs:   mean        expected corr. count per sample w/ 0% corr.  *
*              norm_const  mult. scale factor, where 10000 = 100% corr. *
*                                                                       *
*   First version                             rjc  2001.11.7            *
************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "ff_misc_if.h"

#define min(a,b) ((a<b) ? a : b)
#define max(a,b) ((a>b) ? a : b)

void calc_normalization (int sb,
                         int pol,
                         struct data_corel *datum,
                         struct type_pass *pass,
                         double *mean,
                         double *norm_const)
    {
    extern struct type_param param;

    int ref_index[2][4] = {0, 2, 0, 2, 1, 3, 1, 3},
        rem_index[2][4] = {0, 2, 2, 0, 1, 3, 3, 1},
        product[4][4] = {6, 4, 2, 0, 4, 3, 3, 2, 2, 3, 3, 4, 0, 2, 4, 6},
        i,
        j,
        k;

    double scale, 
           c0, 
           c100,
           cdiff,
           x[4],
           y[4];

         /* the normalization constant contains an arbitrary factor of 10,000,
          a rotator correction factor of pi/(4 cos pi/8), a hardware
          fractional bit correction of 1/(1-pi**2 / 288), and a factor of
          1.035 to compensate for hardware bit shifting */

                                    /* Autocorrelation case, no rotator or bitshift
                                     * corrections are necessary */
    if (pass->autocorr) 
        scale = 10000.;
    else
        scale = 10000. * 0.88027541 * 1.035;


    if (param.use_sample_cnts)
        {
                                    /* form normalized fractions in each of 4 states */

                                    /* first, get fractions from the appropriate sideband
                                     * and polarization, for reference and remote */
        k = ref_index[sb][pol];
        x[0] = datum->ref_sdata.bigneg[k];
        x[1] = datum->ref_sdata.neg[k];
        x[2] = datum->ref_sdata.pos[k];
        x[3] = datum->ref_sdata.bigpos[k];

        k = rem_index[sb][pol];
        y[0] = datum->rem_sdata.bigneg[k];
        y[1] = datum->rem_sdata.neg[k];
        y[2] = datum->rem_sdata.pos[k];
        y[3] = datum->rem_sdata.bigpos[k];


                                    /* given the sampler statistics, find the expected
                                     * correlator product for 0% and 100% correlation */
        c0 = 0.0;
        for (i=0; i<4; i++)
            for (j=0; j<4; j++)
                c0 += x[i] * y[j] * product[i][j];
        
        c100 = 3 + 2 * min(x[3],y[3]) + max(x[3],y[3]) 
                 + 2 * min(x[0],y[0]) + max(x[0],y[0]);
        
        cdiff = c100 - c0;
                                    /* trap out potential 0 divide (shouldn't happen)
                                     * ... or slivers rjc 2005.4.7 */
        if (cdiff <= 0.1)
            {
            msg ("normalization error! c100 = %g", 2, c100);
            cdiff = 1.089;          /* just use value from default statistics */
            }
        else
            msg ("c0 %g c100 %g", -2, c0, c100);


        if (c0)                     /* When either or both stations have all 0's for
                                     * sampler counts, we use the nominal values, as
                                     * though sampler stats were not even requested, so
                                     * we will skip the following calculations */

            {
                                    /* factor of 0.971891 (for 2 bit samples, or
                                     * 1.568168 for 1 bit) was found numerically, for
                                     * nominal threshold settings of 0.91 sigma, and
                                     * is only correct in the weak signal limit, and
                                     * for the nominal settings. It is is error by
                                     * 1 part in 500 for significantly different
                                     * thresholds */
            if (pass->autocorr)
                scale /= cdiff;
            else if (param.bits_sample[0] == 1)
                scale *= 1.568168 / cdiff;
            else
                scale *= 0.971891 / cdiff;

            c0 = 3.0;               /* fringe rotator serves to "mix up" sampler
                                     * states of reference station, so there is
                                     * no bias. This assumption would break down
                                     * for very low fringe rates (few / AP). */
            }
        }
        
    if (param.use_sample_cnts == FALSE || c0 == 0.0)
        {
                                    /* normalize assuming nominal clipper levels */
        c0 = 3.0;                   /* no DC bias */

        if (param.bits_sample[0] == 1) // assumes both same bits/sample for xf case
            {
            if (pass->autocorr)
                scale *= 0.333333;
            else
                scale *= 0.523598;  /* pi/2 Van Vleck clipping correction divided
                                       by 3 since we always get 0's or 6's */
            }
        else
            {
            if (pass->autocorr)
                scale *= 0.918;     /* use nominal 0.91 sigma threshhold */
            else
                scale *= 0.892898;  /* from numerical calculation of weak signal case,
                                       assuming AGC set to nominal 0.91 sigma */
            }
        }
        
    *mean = c0;
    *norm_const = scale;
    }
