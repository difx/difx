/************************************************************************/
/*                                                                      */
/*  Fills in a type_210 record                                          */
/*                                                                      */
/*      Inputs:         via externs                                     */
/*                                                                      */
/*      Output:         t210        Filled in type_210 record           */
/*                                                                      */
/* Created 8 March 2000                                                 */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "vex.h"
#include "pass_struct.h"
#include "param_struct.h"

#define pi 3.141592654

int
fill_210 (/* pass, status, t210) */
struct type_pass *pass,
struct type_status *status,
struct type_210 *t210)
    {
    int i;
    double c_mag(), c_phase();

    clear_210 (t210);
                                        /* Precalculated in make_plotdata() */
    for (i=0; i<pass->nfreq; i++)
        {
        t210->amp_phas[i].ampl = (float)c_mag (status->fringe[i]) / 10000.0;
        t210->amp_phas[i].phase = (float)c_phase (status->fringe[i]) * 180.0 / pi;
        }

    return (0);
    }

