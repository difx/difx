/************************************************/
/*  Subroutine to transform to delay rate       */
/*  domain. Calculates fringe rate spectrum     */
/* then grids it into delay rate.               */
/*                                              */
/*      8/2/91          - cmn                   */
/* 2012.1.4 - rjc - remove pcal rotation here   */
/************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "type_comp.h"
#include "param_struct.h"
#include "pass_struct.h"

delay_rate (pass, fr, rate_spectrum)
struct type_pass *pass;
int fr;
complex rate_spectrum[MAXAP];
    {
    complex apval, a, fringe_spect[MAXAP*2], X[MAXAP*2];
    complex s_mult(), c_mult(), c_add(), c_zero(), c_exp();
    double c_mag();
    int fl, L, ap, np, i, j, l_int, size, l_int2;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    double b, l_fp, frac;
    struct freq_corel *pd;
    struct data_corel *datum;
    extern struct type_param param;
    extern struct type_status status;

    pd = pass->pass_data + fr;
    
    np = status.drsp_size;              /* np = # of ap's in FFT < MAXAP */
    
    size = np * 4;                      /* This is size of FFT */
                                        /* Smaller delay rate spectrum option */
    if (size > MAXAP*2) size = MAXAP*2;
    status.f_rate_size = size;

                                        /* Fill data array */
    for (i = 0; i < size; i++) X[i] = c_zero();
    for (ap = 0; ap < pass->num_ap; ap++)
        {
        datum = pd->data + ap + pass->ap_off;
        apval = datum->sbdelay[status.lag];
                                        /* Weight by fractional AP */
        frac = 0.0;
        if (datum->usbfrac >= 0.0) frac  = datum->usbfrac;
        if (datum->lsbfrac >= 0.0) frac += datum->lsbfrac;
                                        /* When both sidebands added together, */
                                        /* we use the mean fraction */
        if ((datum->usbfrac >= 0.0) && (datum->lsbfrac >= 0.0)) frac /= 2.0;
        X[ap] = s_mult (apval, frac);
        }
     
    FFT1 (X, size, 1, X, 1);

    for (i = 0; i < size; i++)
        {
        j = i - size / 2;
        if (j < 0) j += size;
        fringe_spect[i] = X[j];
        }
        
    b = (pass->pass_data[fr].frequency / param.ref_freq) * size / np;
/*              / pass->pass_data[0].frequency) * size / np;  */

                                        /* Grid fringe rate spectrum to delay */
                                        /* rate spectrum. rate[L] is interpolated */
                                        /* from fringe[l_int] & fringe[l_int2] */
    for (L = 0; L < np; L++)
        {
        l_fp = fmod ((L - (np/2) ) * b + (size * 1.5) , (double)size) ;
        l_int = (int)l_fp;
        l_int2 = l_int+1;
        if (l_int < 0) l_int = 0;
        if (l_int2 > (size-1)) l_int2 = size - 1;
        rate_spectrum[L] = s_mult (fringe_spect[l_int], (1.0 - l_fp + l_int));
        rate_spectrum[L] = c_add (rate_spectrum[L], s_mult (fringe_spect[l_int2],
                                                        (l_fp - l_int)));
        msg("fr %d cmag(rate_spectrum[%d]) %f",-3,fr,L,c_mag(rate_spectrum[L]));
        }
     }
