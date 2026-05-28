/************************************************************************/
/*                                                                      */
/* This routine takes a filled in array of codata structures, and fits  */
/* theoretical curves to each baseline.  It uses these fits to either   */
/* declare the coherence times undetermined, or to fill in the          */
/* coherence time elements of the data record.  It also fills in the    */
/* fitted amplitude and SNR values for plotting purposes.               */
/*                                                                      */
/*      Inputs:         codata          struct array filled with data   */
/*      Output:         codata          Fitting information added       */
/*                                                                      */
/* Created October 5 1995 by CJL                                        */
/* Hacked up Apr 26 2026 by GBC                                         */
/************************************************************************/
#include <math.h>
#include "cohfit.h"
#include "mk4_afio.h"
#include "mk4_util.h"

/* for the moment, some last minute sanity checks, and default cotimes */
int numptscheck(cosumary *codatum)
    {
    int npt;
    codatum->datum->noloss_cotime = -2.0;
    codatum->datum->srch_cotime   = -2.0;
    for (npt=0; npt<MAX_NSEGLEN; npt++)
         if (codatum->ampl[npt] == 0) break;
    if (npt != codatum->nsegtime)
        {
        msg("Developer error: npt %d != nsegtime %d", 2,
            npt, codatum->nsegtime);
        return(-1);
        }
    if (npt < 4)
        {
        msg("Not enough points (%d) to do anything", 3, npt);
        return(-2);
        }
    npt = codatum->nsegtime;
    return(npt);
    }

/* provide time of cohfraction*maxampl in the different cases */
double compute_cotime(cosumary *codatum)
    {
    double noloss_cotime;
    if (codatum->bestamp & FITOPT_AMP_PS) {
        /* solve t>break for t of cohfraction of plateau amplitude */
        double exponent =
            - codatum->cohereloss * codatum->plateau / codatum->slope;
        noloss_cotime = codatum->breakpoint * pow (10.0, exponent);
        if (noloss_cotime < 1.0) noloss_cotime = 1.0;
        noloss_cotime = floor(noloss_cotime);
        msg("plateau-slope noloss_cotime = %f", 2, noloss_cotime);
    } else if (codatum->bestamp & FITOPT_AMP_PO) {
        /* punt: no way to extrapolate past the end */
        noloss_cotime = codatum->seglen[codatum->nsegtime-1];
        msg("plateau-only noloss_cotime = %f", 2, noloss_cotime);
    } else if (codatum->bestamp & FITOPT_AMP_SO) {
        /* same expression given how we have defined plateau and breakpoint */
        double exponent =
            - codatum->cohereloss * codatum->plateau / codatum->slope;
        noloss_cotime = codatum->breakpoint * pow (10.0, exponent);
        if (noloss_cotime < 1.0) noloss_cotime = 1.0;
        noloss_cotime = noloss_cotime = floor(noloss_cotime);
        msg("slope-only noloss_cotime = %f", 2, noloss_cotime);
    } else {
        msg("Failure to deal with amplitude", 2);
        noloss_cotime = -1.0;
    }
    return( floor(codatum->ampl_cotime = noloss_cotime) );
    }

void fit_codata (cosumary codata[], examdata *epatdatap)
    {
    int base, nbase, npt, i, rv;
    cosumary *codatum;
                                        /* How many baselines are there? */
    nbase = 0;
    while (codata[nbase].datum != NULL) nbase++;
                                        /* Loop over all baselines in codata */
    for (base=0; base<nbase; base++)
        {
        msg("", 2);
        msg("New baseline: fit_codata(%d of %d)", 2, base, nbase);
        codatum = codata + base;
                                        /* How many points are we fitting? */
        if ((npt = numptscheck(codatum)) < 3) return;

                                        /* Delegate fitting to subroutines */
        msg("", 2);
        msg("Amplitude fit for '%s' pol '%s'", 2,
            fringename(codatum->datum), codatum->datum->polarization);
        if ((rv = fit_ampl (codatum, npt, epatdatap->gslegacy)) != 0)
            {
            codatum->datum->noloss_cotime = -1;
            msg("Error %d: ps=%g|po=%g|so=%g fitting noloss cotime for '%s'",
                2, rv, codatum->redchisq[0],
                codatum->redchisq[1], codatum->redchisq[2],
                fringename (codatum->datum));
            }
        else 
            {
            /* estimate the time of cohereloss amplitude */
            codatum->datum->noloss_cotime = compute_cotime(codatum);
            }

        if (normalize_snr (codatum, npt) != 0)
            {
            /* probably already reported */
            codatum->datum->srch_cotime = -1;
            }

        msg("", 2);
        msg("SNR fit for '%s'", 2, fringename (codatum->datum));
        if ((codatum->datum->srch_cotime = fit_msnr (codatum, npt)) <=0) 
            {
            codatum->datum->srch_cotime = -1;
            msg("Error fitting srch_cotime for '%s'", 2,
                                        fringename (codatum->datum));
            }
        else
            {
            msg("srch_cotime = %d",   2, codatum->datum->srch_cotime);
            msg("noloss_cotime = %d", 2, codatum->datum->noloss_cotime);
            codatum->orig_srch = codatum->datum->srch_cotime;
            if (codatum->datum->srch_cotime < codatum->datum->noloss_cotime)
                codatum->datum->srch_cotime = codatum->datum->noloss_cotime;
            }
            // codatum->snr_cotime = codatum->datum->srch_cotime;

                                        /* Impose upper limit */
        if (codatum->datum->noloss_cotime > codatum->maxcotime)
            codatum->datum->noloss_cotime = codatum->maxcotime;
        if (codatum->datum->srch_cotime > codatum->maxcotime)
            codatum->datum->srch_cotime = codatum->maxcotime;
                                        /* Flagging message */  
        if ((codatum->datum->srch_cotime   == -2.0) || 
            (codatum->datum->noloss_cotime == -2.0))
                msg("Cofit never fit this scan/baseline ...",2);
        if ((codatum->datum->srch_cotime   == -1.0) || 
            (codatum->datum->noloss_cotime == -1.0))
                msg("Cofit removing this scan/baseline from output ...",2);
        if (codatum->examlen)
            {
            msg("Datum %d exported as %s.{gnu,pdf}", 2,
                base, codatum->exampat);
            exam_file(codatum, base, epatdatap);
            }
        }
    return;
    }

/*
 * eof vim:nospell
 */
