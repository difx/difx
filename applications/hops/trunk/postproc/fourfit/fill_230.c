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
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

int
fill_230 (/* pass, param, fr, ap, t230) */
struct type_pass *pass,
struct type_param *param,
int fr,
int ap,
struct type_230 *t230)
    {
    struct data_corel *datum;
    complex work_array[4 * MAXLAG], value;
    complex c_zero(), c_mult(), c_exp();
    double theta;
    int i, j, lag, nl;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    extern struct type_status status;

    clear_230 (t230);
    
    datum = pass->pass_data[fr].data + ap;

    t230->nspec_pts = 2 * param->nlags;
    t230->frq = fr;
    t230->ap = ap;
    t230->usbweight = datum->usbfrac;
    t230->lsbweight = datum->lsbfrac;
    if (datum->flag == 0) t230->usbweight = t230->lsbweight = -1.0;

    for (i = 0; i < 4 * MAXLAG; i++) work_array[i] = c_zero();
                                        /* Fill padded work array */
    nl = param->nlags;
    for (lag = 0; lag < nl * 2; lag++)
        {
        j = lag - nl;
        if (j < 0) j += 4 * nl;
        value = datum->sbdelay[lag];
                                        /* Remove mean phasecal */
        theta = (status.pc_phase[fr][1][stnpol[1][pass->pol]] 
               - status.pc_phase[fr][0][stnpol[0][pass->pol]]);
        work_array[j] = c_mult (value, c_exp (theta));
        }
                                        /* FFT sband delay to xpower spectrum */
    FFT1 (work_array, 4 * nl, 1, work_array, 1);
                                        /* Sort back into xpower array */
    for (i = 0; i < 2*nl; i++)
       {
       j = nl - i;
       if (j < 0) j += 4*nl;
       t230->xpower[i] = work_array[j];
       }

    return (0);
    }
