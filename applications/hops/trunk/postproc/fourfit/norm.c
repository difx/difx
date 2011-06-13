/************************************************************************/
/*                                                                      */
/* Despite its name, this routine does more than simply normalise the   */
/* correlation coefficients.  It also combines sidebands and transforms */
/* to singleband delay, and calculates phasecal values.  The results    */
/* are placed in a data_corel structure associated with each ap         */
/*                                                                      */
/* Created July 1991 by Cris Niell                                      */
/* Overhauled and beautified by CJL April 29, 1992                      */
/* Modified for two-bit operation.                   rjc 93.3.22        */
/* Change scale factor from 30000 to 10000.          rjc 94.1.24        */
/* Mult. scale by 4/3 since Mk4 has valid 0 rotator  rjc 99.8.3         */
/* Apply different normalization for 2 bit case      rjc 01.01.19       */
/* Break out normalization code into separate func.  rjc 01.11.7        */
/*                                                                      */
/************************************************************************/

/************************************************************************/
/*                                                                      */
/* Substantial rewrite needed for Mk4, including support for variable   */
/* number of lags, multiple polarizations.                              */
/*                                                                      */
/*      Inputs:     pass        Contains various essential parameters   */
/*                  pol         0,1,2,3 = LL,RR,LR,RL                   */
/*                  fr          fourfit frequency channel number        */
/*                  ap          Sequential AP number                    */
/*                                                                      */
/*      Output:     t_pcal      Does this belong here?                  */
/*                                                                      */
/* Overhauled 27 April 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

                                        /* minimum #bits for an AP to count */
#define SLIVER   0x1000
#define TWO_32   4294967296.0
#define SYSCLK_S 32.e6                  /* number of sysclks / sec */

norm (pass, pol, fr, ap)
struct type_pass *pass;
int pol, fr, ap;
    { 
    struct type_120 *rawdatum;
    struct freq_corel *fdata;
    struct data_corel *datum;
    int flagbit, sb, st, i, rev_i, j, l, m, intshift, ct, nlags;
    int cosbits, coscor, sinbits, sincor;
    complex xcor[4*MAXLAG], xp_spec[2*MAXLAG], S[4*MAXLAG];
    complex c_zero(), conju(), c_mult(), c_add(), c_exp(), s_mult();
    double c_mag ();
    float  fraction, count_frac;
    unsigned short bitshift;
    double theta, shift, dd, norm_const, fb_fact, fb_amp_ratio();
    double samp_per_ap, corrfact, rawcorr, factor, mean;
    int flagmask;
    extern struct type_param param;
    extern struct type_status status;



    nlags = param.nlags;
    samp_per_ap = param.acc_period / param.samp_period;

                                        /* Point to current frequency */
    fdata = pass->pass_data + fr;
                                        /* Convenience pointer */
    datum = fdata->data + ap;
                                        /* Initialize */
    for (i = 0; i < nlags*4; i++) S[i] = c_zero();
    datum->sband = 0;
                                        /* -1.0 means no data, not zero weight */
    datum->usbfrac = -1.0;
    datum->lsbfrac = -1.0;
                                        /*  sideband # -->  0=upper , 1= lower */
    for (sb = 0; sb < 2; sb++) 
        {
        if (sb) 
            flagmask = LSB_FLAG << 2*pol;
        else
            flagmask = USB_FLAG << 2*pol;
                                        /* If no data, skip over this sideband*/
        if ((datum->flag & flagmask) == 0)
            continue;
                                        /* Convenience pointer */
                                        /* Pluck out the requested polarization */
        switch (pol)
            {
            case POL_LL: rawdatum = datum->apdata_ll[sb];
                         break;
            case POL_RR: rawdatum = datum->apdata_rr[sb];
                         break;
            case POL_LR: rawdatum = datum->apdata_lr[sb];
                         break;
            case POL_RL: rawdatum = datum->apdata_rl[sb];
                         break;
            }
                                        /* Keep track of sideband info */
        if (sb) 
            datum->sband -= 1; /* LSB */
        else    
            datum->sband += 1;
            
        if (param.corr_type == MK4HDW)
            {
                                        /* Calculate normalization quantities */
            calc_normalization (sb, pol, datum, pass, &mean, &norm_const);
        
                                        /* Next, compute correlation coefficients */
                                        /* 10,000 = 100% correlated */
            for(i=0;i<nlags*4;i++)
                xcor[i]=c_zero();
                                        /* Loop over lags 
                                         *  i source index for cross correlation 
                                         *  m    "     "    "  auto       "
                                         *  l destination index for both */
            for (i = 0; i < nlags; i++)
                {
                                        /* re-sort so that lag = 0 is xcor[0] */
                                        /* Reverse lag direction for Mk4 */
                l = nlags/2 - i;
                if (l < 0) l += nlags*2;
                                        /* Form normalized correlation vector */
                                        /* Account for different formats */
                if (rawdatum -> type == COUNTS_GLOBAL)
                    {
                    cosbits = rawdatum->ld.cg.cosbits;
                    coscor = rawdatum->ld.cg.lags[i].coscor;
                    sinbits = rawdatum->ld.cg.sinbits;
                    sincor = rawdatum->ld.cg.lags[i].sincor;
                    }
                else if (rawdatum -> type == COUNTS_PER_LAG)
                    {
                    cosbits = rawdatum->ld.cpl[i].cosbits;
                    coscor = rawdatum->ld.cpl[i].coscor;
                    sinbits = rawdatum->ld.cpl[i].sinbits;
                    sincor = rawdatum->ld.cpl[i].sincor;
                    }
                else if (rawdatum -> type == AUTO_GLOBAL)
                    {                       /* autocorrelation lags are duplicated here,
                                           since 2 values of i map to the same m */
                    if (i < nlags/2) m = nlags/2 - i;
                    else m = i - nlags/2;
                                        /* Kluge to avoid zeros */
                                        // also avoid duplicate entries  rjc 2007.8.13
                    if (i == 0)
                        continue;
                                        // old code: if (m == nlags/2) m -= 1;
                    cosbits = rawdatum->ld.ag.cosbits;
                    coscor = rawdatum->ld.ag.coscor[m];
                    sinbits = 0;
                    sincor = 0;
                    }
                else if (rawdatum -> type == AUTO_PER_LAG)
                    {
                    if (i < nlags/2) m = nlags/2 - i;
                    else m = i - nlags/2;
                                        /* Kluge to avoid zeros */
                                        // also avoid duplicate entries  rjc 2007.8.13
                    if (i == 0)
                        continue;
                                        // old code: if (m == nlags/2) m -= 1;
                    cosbits = rawdatum->ld.apl[m].cosbits;
                    coscor = rawdatum->ld.apl[m].coscor;
                    sinbits = 0;
                    sincor = 0;
                    }
                else
                    {
                    msg ("Unsupported correlation type %d", 2, param.cormode);
                    return;
                    }
                                        /* check for (and discard) lags with no counts */
                if ((cosbits == 0) || ((sinbits == 0) && (! pass->autocorr)))
                    {
                    status.zero_errors++;
                    msg ("marking ap %d for a zero error", 0, ap);
                                        /* mark this sideband & pol piece of this AP bad */
                    datum->flag &= ~(1 << (2 * pol + sb));
                    break;                  /* break out of loop over lags  */
                    }            
                                     
                if ((cosbits < SLIVER) || ((sinbits < SLIVER) && (! pass->autocorr)))
                    {
                    status.sliver_errors++;
                    msg ("marking ap %d for a sliver error", 0, ap);
                                        /* mark this sideband & pol piece of this AP bad */
                datum->flag &= ~(1 << (2 * pol + sb));
                break;                  /* break out of loop over lags  */
                }
                                        /* Convert lag values to true correlation
                                         * coefficients. Factor of 8 is due to hidden
                                         * 3 lower order bits in accumulator */
                                        /* Proper autocorrelation normalization is */
                                        /* lag-dependent, Need computation here */
                xcor[l].re = norm_const * (8.0 * coscor - mean * cosbits) / cosbits;
                if (pass->autocorr) xcor[l].im = 0.0;
                                        /* Conjugate if USB, not if LSB */
                else if (sb == 0)
                    xcor[l].im = -norm_const * (8.0 * sincor - mean * sinbits) / sinbits;
                else if (sb == 1)
                    xcor[l].im =  norm_const * (8.0 * sincor - mean * sinbits) / sinbits;

                                        /* Check for wild values, zero out and */
                                        /* flag for later accounting */
                if ((xcor[l].re > param.cor_limit) || (xcor[l].re < -param.cor_limit) 
                    || (xcor[l].im > param.cor_limit) || (xcor[l].im < -param.cor_limit)) 
                    {
                    status.large_errors++;
                                        /* mark this sideband & pol piece of this AP bad */
                    datum->flag &= ~(1 << (2 * pol + sb));
                    break;                  /* break out of loop over lags  */
                    }
                                        /* Accumulate data fractions */
                                        /* Makes use of fact that cosbits */
                                        /* = sinbits for cross-corr */
                if (sb) 
                    {
                    if (datum->lsbfrac < 0.0) datum->lsbfrac = 0.0;
                    datum->lsbfrac += 
                        (double)(cosbits)/(samp_per_ap * nlags);
                    }
                else 
                    {
                    if (datum->usbfrac < 0.0) datum->usbfrac = 0.0;
                    datum->usbfrac += 
                        (double)(cosbits)/(samp_per_ap * nlags);
                    }
                }                           /* end of loop over i (lags) */
            
                                        /* In case large error zapped this datum ... */
            if ((datum->flag & (1 << (2 * pol + sb))) == 0)
                {
                msg ("skipping data for ap %d pol %d sb %d", -2, ap, pol, sb);
                continue;                   /* skip over rest of the sideband loop */
            }
                                        /* Increment a.p. counts */
                                        /* Integer counts ... */
            (status.ap_num[sb][fr])++;
            status.total_ap++;
                                        /* Microedited counts */
            if (sb) 
                {
                status.ap_frac[sb][fr] += datum->lsbfrac;
                status.total_ap_frac += datum->lsbfrac;
                }
            else 
                {
                status.ap_frac[sb][fr] += datum->usbfrac;
                status.total_ap_frac += datum->usbfrac;
                }
                                        /* Avg to find center of scan */
            status.epoch_off_cent += (double)ap;  
                                        /* FFT to X-power spectrum  */
            FFT1 (xcor, nlags*2, 1, xp_spec, 1);

                                        /* apply spectral filter, if specified */
            apply_passband (sb, fdata, xp_spec, nlags*2);

                                        /* Adjust for Bit-shift */
                                        /* First, decode total bitshift shift */
            shift = fabs (rawdatum->delay_rate / TWO_32 * param.acc_period * SYSCLK_S);
                                        /* Get fractional part */
            if (param.bocfs_per_ap % 2)     /* odd # of cf's/AP; must 
                                           extrapolate fractional delay to cf center */
                {
                fraction = (rawdatum->fr_delay + 
                            0.5 * param.bocf_period * rawdatum->delay_rate) / TWO_32;
                                        /* adjust fraction to lie in [-0.5, 0.5] */
                if (fraction > 0.5)
                    fraction -= 1.0;
                
                if (fraction < -0.5)
                    fraction += 1.0;
                }
            else
                                        /* even # of corr. frames per AP */
                fraction = rawdatum->fr_delay / TWO_32;

                                        /* Calculate actual number of shifts, from
                                           AEER's 1988 memo */
            intshift =  (int) (shift * 0.5 + fraction + 0.5)
                      + (int) (shift * 0.5 - fraction + 0.5);

            if ((intshift % 2) == 0)
                dd = 0.0;
            else if (fraction > 0.0)
                dd = 0.5;
            else 
                dd = -0.5;
        // printf ("shift %lg intshift %d dd %lf fraction %f fr_delay %x delay_rate %x\r\n",
        //          shift, intshift, dd, fraction, rawdatum->fr_delay, rawdatum->delay_rate);
                                        /* Convert to angle, avoid zero division */
            if (shift == 0.0) shift = 1.0;

                                        /* switched sign of theta  rjc 99.11.19 */
            theta = - M_PI / nlags * (fraction - dd) * (1 - intshift / shift);

                                        // retroactive fix for non-zero values of delay and
                                        // delay_rate coming from correlator in the case of
                                        // an autocorrelation           rjc 2007.8.14
            if (rawdatum->type == AUTO_GLOBAL || rawdatum->type == AUTO_PER_LAG)
                {
                fraction = 0.0;
                shift = 1.0;
                theta = 0.0;
                }
                                        /* Factor to correct AP amplitudes by */
                                        /* Maximum of +- 0.2% */
            fb_fact = fb_amp_ratio ((double) fraction, shift);

        /* printf ("*** theta %lf fb_fact %lf sb %d nlags %d\r\n", theta, fb_fact, sb, nlags); */
            }
        else                            // difx (spectral) mode; just copy spectrum in
            {
            if (rawdatum -> type != SPECTRAL)
                {
                msg ("Conflicting correlation type %d", 2, rawdatum->type);
                return;
                }
            
            status.ap_num[sb][fr]++;
            status.total_ap++;
            fb_fact = 1.0;          // may want to re-examine this assumption

            if (sb) 
                {
                datum->lsbfrac = 1.0;
                status.ap_frac[sb][fr] += datum->lsbfrac;
                status.total_ap_frac += datum->lsbfrac;
                }
            else 
                {
                datum->usbfrac = 1.0;
                status.ap_frac[sb][fr] += datum->usbfrac;
                status.total_ap_frac += datum->usbfrac;
                }

            for (i=0; i<nlags; i++)
                {
                xp_spec[i].re = rawdatum->ld.spec[i].re;
                xp_spec[i].im = rawdatum->ld.spec[i].im;
                }
            }
                                        /* Put sidebands together.  For each sb, */
                                        /* the Xpower array, which is the FFT across */
                                        /* lags, is symmetrical about DC of the */
                                        /* sampled sideband, and thus contains the */
                                        /* (filtered out) "other" sideband, which */
                                        /* consists primarily of noise.  Thus we only */
                                        /* copy in half of the Xpower array */
                                        /* Weight each sideband by data fraction */

        for (i = 1; i < nlags; i++)
            {
            if (sb == 0)
                {
                factor = fb_fact * datum->usbfrac;
                S[i] = s_mult (c_mult (xp_spec[i], 
                        c_exp (theta * (double)(i-nlags/2))),factor);
                }
                                        /* LSB: conjugate, add phase offset */
            else
                {
                factor = fb_fact * datum->lsbfrac;
                S[nlags*4 - i] = s_mult (conju (c_mult (xp_spec[i], 
                               c_exp (theta * (double)(i-nlags/2) 
                             + status.lsb_phoff[0] - status.lsb_phoff[1]))),factor);
                }
            }
        }       /* End sideband loop */
                                        /* Normalize data fractions */
                                        /* The resulting sbdelay functions which */
                                        /* are attached to each AP from this point */
                                        /* on reflect twice as much power in the */
                                        /* double sideband case as in single sideband. */
                                        /* The usbfrac and lsbfrac numbers determine */
                                        /* a multiplicative weighting factor to be */
                                        /* applied.  In the double sideband case, the */
                                        /* factor of two is inherent in the data values */
                                        /* and additional weighting should be done */
                                        /* using the mean of usbfrac and lsbfrac */
    factor = 0.0;
    if (datum->usbfrac >= 0.0) factor += datum->usbfrac;
    if (datum->lsbfrac >= 0.0) factor += datum->lsbfrac;
    if ((datum->usbfrac >= 0.0) && (datum->lsbfrac >= 0.0)) factor /= 2.0;
    if (factor > 0.0) factor = 1.0 / factor;
    msg ("usbfrac %1f lsbfrac %1f factor %1f", -2, datum->usbfrac, datum->lsbfrac, factor);
    for (i=0; i<4*nlags; i++) S[i] = s_mult (S[i], factor);

                                        /* Collect the results */
    if(datum->flag != 0)
        {
                                        /* FFT to single-band delay */
        FFT1 (S, nlags*4, 1, xcor, 1);
                                        /* Place SB delay values in data structure */
        for (i = 0; i < nlags*2; i++)
            {
                                        /* Translate so i=nlags is central lag */
            j = i - nlags;
            if (j < 0) j += nlags*4;
                                        /* re-normalize back to single lag */
                                        /* (property of FFTs) */
            datum->sbdelay[i] = s_mult (xcor[j], 1.0 / (nlags - 1.0));
            }
         }
                                        /* No data */
    else
        for (i = 0; i < nlags*2; i++) datum->sbdelay[i] = c_zero();
    }


/*******************************************************************************
* fb_amp_ratio() calculates the ratio of this AP's amplitude correction to the *
*              mean correction that is applied globally. See RJC's derivation. *
* f: fractional bit error at center of AP; in range [-.5,.5]                   *
* r: bit shift rate in bitshifts per AP                                        *
*******************************************************************************/
double fb_amp_ratio (f,r)
double f,r;
    
    {
    double fp1,fp2,fp3,
           fm1,fm2,fm3,
           f2,r2,fact;
double pi2 = 9.869604401;

    f2 = f * f;                          /* pre-calculate useful factors */
    r2 = r * r;

    fp1 = (int) floor (f + r / 2 + 0.5);
    fp2 = fp1 * fp1;          /* name is floor of expression, with plus sign, to 2nd power */
    fp3 = fp1 * fp2;

    fm1 = (int) floor (f - r / 2 + 0.5);
    fm2 = fm1 * fm1;
    fm3 = fm1 * fm2;

    fact = 28.1805 * r /
       (4 * fp3 - 6 * ( 2 * f + r) * fp2 + (12 * f2 + 12 * f * r + 3 * r2 - 1) * fp1
      - 4 * fm3 + 6 * ( 2 * f - r) * fm2 - (12 * f2 - 12 * f * r + 3 * r2 - 1) * fm1
      - r * (12 * f2 + r2 - 29.1805));
    return fact;
    }   
