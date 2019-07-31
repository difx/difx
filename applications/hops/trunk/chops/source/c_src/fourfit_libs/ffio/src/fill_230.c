/************************************************************************/
/*                                                                      */
/* This fills in a type 230 record for 1 channel and 1 ap from the      */
/* singleband delay function stored in the pass structure.              */
/*                                                                      */
/* Created April 23 2001 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <complex.h>
#include <fftw3.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

int
fill_230 (
struct type_pass *pass,
struct type_param *param,
int fr,
int ap,
struct type_230 *t230)
    {
    struct data_corel *datum;
    complex value;
    static complex work_array[4 * MAXLAG];
    double theta;
    int i, j, lag, nl;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    extern struct type_status status;
    static int fftsize = 0;
    static fftw_plan fftplan;

    clear_230 (t230);
    
    datum = pass->pass_data[fr].data + ap;

    t230->nspec_pts = 2 * param->nlags;
    t230->frq = fr;
    t230->ap = ap;
    t230->usbweight = datum->usbfrac;
    t230->lsbweight = datum->lsbfrac;
    if (datum->flag == 0) 
        t230->usbweight = t230->lsbweight = -1.0;

    for (i = 0; i < 4 * MAXLAG; i++) 
        work_array[i] = 0.0;
                                        /* Fill padded work array */
    nl = param->nlags;
    if (fftsize != 4 * nl)
        {
        fftsize = 4 * nl;
        fftplan = fftw_plan_dft_1d (fftsize, work_array, work_array, FFTW_FORWARD, FFTW_MEASURE);
        }

    for (lag = 0; lag < nl * 2; lag++)
        {
        j = lag - nl;
        if (j < 0) 
            j += 4 * nl;
        // value = datum->sbdelay[lag];
                                        /* Remove mean phasecal */
        // theta = (status.pc_phase[fr][1][stnpol[1][pass->pol]] 
        //       - status.pc_phase[fr][0][stnpol[0][pass->pol]]);
        // work_array[j] = c_mult (value, c_exp (theta));
        work_array[j] = datum->sbdelay[lag];
        }
                                        /* FFT sband delay to xpower spectrum */
    fftw_execute (fftplan);
                                        /* Sort back into xpower array */
    for (i = 0; i < 2*nl; i++)
       {
       j = nl - i;
       if (j < 0) j += 4*nl;
       t230->xpower[i] = work_array[j];
       }

    return (0);
    }
