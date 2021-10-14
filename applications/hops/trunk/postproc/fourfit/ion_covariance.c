/*********************************************************
*  Subroutine to perform ionospheric covariance analysis *
*                                                        *
*  refactor initial code from make_plotdata 2019.9.13 rjc*
*********************************************************/

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <math.h>
#include <stdio.h>
#include "hops_complex.h"

void ion_covariance (struct type_pass *pass)
    {
    int i, j, fr;
    double sigma_fr,
           fk,
           f0,
           w,
           A[3][3],                     // normal equation matrix
           C[3][3];                     // covariance matrix
    const double b = -1.3445;           // for correct TEC units

    extern struct type_status status;
    extern struct type_param param;
                                        // function prototypes
    int minvert (int, double [3][3], double[3][3]);

    for (i=0; i<3; i++)                 // pre-clear the normal matrix
        for (j=0; j<3; j++)
            A[i][j] = 0.0;

    for (fr = 0; fr < pass->nfreq; fr++)
        {
                                        // increment normal equations
        sigma_fr = sqrt ((double)pass->nfreq) * status.delres_max /
                       (2.0 * M_PI * status.snr * cabs (status.fringe[fr]));
                                        // coefficient matrix weight
        w = 1.0 / (sigma_fr * sigma_fr);
                                        // convenience variables to match rjc memo
        fk = 1e-3 * pass->pass_data[fr].frequency;
        f0 = 1e-3 * param.ref_freq;     // (GHz)

        A[0][0] += w * (fk - f0) * (fk - f0);
        A[0][1] += w * (fk - f0);
        A[0][2] += w * b * (fk - f0) / fk;
        A[1][1] += w;
        A[1][2] += w * b / fk;
        A[2][2] += w * (b / fk) * (b / fk);
        }
    A[1][0] = A[0][1];                  // fill in rest of symmetric normal matrix
    A[2][0] = A[0][2];
    A[2][1] = A[1][2];

                                        // invert the normal matrix to get covariance matrix
    if (minvert (3, A, C))              // error returned?
        if (status.nion)
            {                           // - yes
            msg ("unable to compute ionosphere errors due to singular matrix", 2);
            }

    for (i=0; i<3; i++)             // std devs. are sqrt of diag of covariance matrix
        status.ion_sigmas[i] = sqrt (C[i][i]);
    for (i=0; i<3; i++)             // normalize covariance to get correlation matrix
        for (j=0; j<3; j++)
            C[i][j] /= (status.ion_sigmas[i] * status.ion_sigmas[j]);
    msg ("ionospheric sigmas: delay %f (ps) phase %f (deg) dTEC %f", 0,
          1e3 * status.ion_sigmas[0], 360 * status.ion_sigmas[1], status.ion_sigmas[2]);
    msg ("ionosphere correlation matrix:\n"
         "%7.3f %7.3f %7.3f\n%7.3f %7.3f %7.3f\n%7.3f %7.3f %7.3f", 1,
         C[0][0], C[0][1], C[0][2], C[1][0], C[1][1], C[1][2], C[2][0], C[2][1], C[2][2]);

    }
