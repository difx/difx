/*
 * $Id$
 *
 * Hook for possible correlation data repair...
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "type_comp.h"

extern complex c_exp(double theta);
extern complex c_mult(complex a, complex b);

static FILE *fx = 0;
static int before = 0;
static int after = 0;
static double factor = 0.0, freqlo = 0.0;

static double get_param_ref_freq(void);

/*
 * one time setup
 */
static int hackage_setup(void)
{
    char *ep;
    int keep = 0;
    fx = fopen("fearfit.data", "w");
    if (!fx) { perror("spectral_hackage:fopen"); exit(1); }
    ep = getenv("HOPS_FEARFIT_BEFORE");
    before = ep ? atoi(ep) : 0;
    keep |= (int)ep;
    fprintf(fx, "## HOPS_FEARFIT_BEFORE=%d\n", before);
    ep = getenv("HOPS_FEARFIT_AFTER");
    after = ep ? atoi(ep) : 0;
    keep |= (int)ep;
    fprintf(fx, "## HOPS_FEARFIT_AFTER=%d\n", after);
    ep = getenv("HOPS_FEARFIT_FACTOR");
    keep |= (int)ep;
    factor = ep ? atof(ep) : 0;
    fprintf(fx, "## HOPS_FEARFIT_FACTOR=%g (us/s)\n", factor);
    ep = getenv("HOPS_FEARFIT_FREQLO");
    freqlo = ep
	   ? (strcmp(ep, "ref_freq") ? atof(ep) : get_param_ref_freq())
	   : 0.0;
    keep |= (int)ep;
    fprintf(fx, "## HOPS_FEARFIT_FREQLO=%g (MHz)\n", freqlo);
    fprintf(fx, "## theta = 2*M_PI * factor * "
		"( freq + ii * sign - freqlo ) * (double)ap\n");
    if (!keep) unlink("fearfit.data");
    return(0);
}

/*
 * Compute rotation angle
 */
static double theta(double freq, int ap, int ii, int sign)
{
    return( 2.0 * M_PI * factor * ( freq + ii * sign - freqlo ) * (double)ap );
}

/*
 * Rotate the spectrum
 */
static void spectral_rotation(complex S[], int nlags, double freq, int ap)
{
    int ii, jj;
    for (ii = 1, jj = 4*nlags - 1; ii < nlags; ii++, jj--) {
	S[ii] = c_mult(S[ii], c_exp(theta(freq,ap,ii,+1)));
	S[jj] = c_mult(S[jj], c_exp(theta(freq,ap,ii,-1)));
    }
}

/*
 * A diagnostic
 */
static void spectral_dump(char *x, complex S[], int nlags, double freq, int ap)
{
    int ii;
    fprintf(fx, "## %s freq %f ap %d\n", x, freq, ap);
    for (ii = 0; ii < nlags*4; ii++)
	if (S[ii].re != 0.0 && S[ii].im != 0.0)
	    fprintf(fx, " %d %g %g\n", ii, S[ii].re, S[ii].im);
    fprintf(fx, "\n\n");
}

/*
 * This function is called on the visibility data of each AP
 * immediately prior to the FFT1(S) call *outside* of the sideband loop.
 * This allows rather general manipulation/examination of the data,
 * and it's completely outside of fourfit's normal processing....
 */
static void spectral_hackage(complex S[], int nlags, double freq, int ap)
{
    static int once = 1;
    if (once) once = hackage_setup();
    if (before) spectral_dump("(before)", S, nlags, freq, ap);
    if (factor) spectral_rotation(S, nlags, freq, ap);
    if (after) spectral_dump("(after) ", S, nlags, freq, ap);
}

#define NORM_HACK_HOOK\
    spectral_hackage(S, nlags, fdata->frequency, ap);
#include "norm.c"

/*
 * param is defined in type_param.h but it's already included by
 * norm.c and it's not #ifdef-wrapped for re-inclusion
 */
static double get_param_ref_freq(void)
{
    extern struct type_param param;
    return(param.ref_freq);
}

/*
 * eof
 */
