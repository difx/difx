/********************************************************************
* apply corrections to video bandpass power spectrum, if specified
*
* nominal model is a cubic polynomial + 1/f term
* for now, file-based corrections are just stubbed in
*
* first written                                     rjc 2019.6.28
*********************************************************************/

//#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "apply_funcs.h"
#include "ff_misc_if.h"
#include <stdio.h>

extern void   msg (char *, int, ...);

void apply_video_bp (hops_complex *xp_spec,
                     int npts,
                     struct type_pass *pass)
    {
    int i;
    double f, deltaf, delphi, a[5];
    extern struct type_param param;
    static double conrad = 0.01745329252;

    if (pass->control.vbp_file[0][0])   // apply file-based vbp corrections (NYI)
        {
        msg ("file-based video bandpass corrections NYI", 2);
        }

    else                                // apply algebraic model vbp corrections
        {
        deltaf = 0.5e-6 / param.samp_period / npts;
                                        // form differenced coefficients
        for (i=0; i<5; i++)
            a[i] = pass->control.vbp_coeffs[i].rem - pass->control.vbp_coeffs[i].ref;
                                        // evaluate and apply the model
        for (i=0; i<npts; i++)
            {
            f = (i + 0.5) * deltaf;
            delphi = a[0]*f*f*f + a[1]*f*f + a[2]*f + a[3] + a[4]/f;
            xp_spec[i] *= cexp (I * delphi * conrad);
            }
        }
    }
