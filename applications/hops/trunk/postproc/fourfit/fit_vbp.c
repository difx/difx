/*********************************************************
*  Subroutine to fit for video bandpass parameters       *
*  with phase model of:                                  *
*  phi = af^3+bf^2+cf+d+e/f                              *
*                                                        *
*  initial code from matlab prototype      2019.9.13 rjc *
*********************************************************/

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <math.h>
#include <stdio.h>
#include "hops_complex.h"

void fit_vbp (int npts)
    {
    int i, j, k;
    double f,                           // video freq of xpower pt (Hz)
           bw,                          // single-sided bandwidth (Hz)
           res,                         // freq spacing of xpower spectrum
           sb,                          // sideband usb = 1, lsb = -1
           wgt,                         // least squares weight
           a[5][5],                     // normal equation matrix
           b[5],                        // RHS of normal equations
           a_inv[5][5],                 // covariance matrix
           x[5],                        // solution vector
           sigma[5];                    // std error of solution vector

    static double conrad = 0.01745329252;

    extern struct type_status status;
    extern struct type_param  param;
    extern struct type_plot   plot;
    extern int msglev;
                                        // function prototypes
    int minvert (int, double [5][5], double[5][5]);

    for (i=0; i<5; i++)                 // pre-clear the normal matrix
        {
        b[i] = 0.0;
        for (j=0; j<5; j++)
            a[i][j] = 0.0;
        }

    bw = 0.25 / status.sbd_sep;         // video bandwidth
    res = bw / npts;                    // spectral resolution

    for (k=0; k<2*npts; k++)
        {
        wgt = cabs(plot.cp_spectrum[k]);
        wgt *= wgt;             // weight is square of xpower amplitude

        if (k < npts)           // LSB
            {
            f = (npts - k) * res;
            sb = -1.0;
            }
        else                    // USB
            {
            f = (k + 1 - npts) * res;
            sb = 1.0;
            }

        for (i=0; i<5; i++)             // form the normal matrix
            for (j=0; j<5; j++)
                {
                a[i][j] += pow (f, 6 - i - j) * wgt;
                                        // also add into the RHS, just once per i & k
                if (j == 0)
                    b[i] += carg (plot.cp_spectrum[k]) / conrad * pow (f, 3 - i) * sb * wgt;
                }
        }

                                    // invert the normal matrix to get covariance matrix
    if (minvert (5, a, a_inv))      // error returned?
        {                           // if so, report it
        msg ("unable to compute vbp model due to singular matrix", 2);
        }

    for (i=0; i<5; i++)
        {
        x[i] = 0.0;
        sigma[i] = sqrt (a_inv[i][i]);
        for (j=0; j<5; j++)         // matrix multiplication of a_inv * b
            x[i] += a_inv[i][j] * b[j];
        }
    for (i=0; i<5; i++)             // normalize covariance to get correlation matrix
        for (j=0; j<5; j++)
            a_inv[i][j] /= (sigma[i] * sigma[j]);
    msg ("vbp_coeffs %7.3f %7.3f %7.3f %7.3f %7.3f", 2,
         x[0], x[1], x[2], x[3], x[4]);
    msg ("      +/-  %7.3f %7.3f %7.3f %7.3f %7.3f", 2,
         sigma[0], sigma[1], sigma[2], sigma[3], sigma[4]);
    if (msglev < 2)
        {
        msg ("video bandpass correlation matrix:", 2);
        for (i=0; i<5; i++)
            msg ("%7.3f %7.3f %7.3f %7.3f %7.3f", 1,
                 a_inv[i][0], a_inv[i][1], a_inv[i][2], a_inv[i][3], a_inv[i][4]);
        }
    }
