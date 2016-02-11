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
/* Overhauled                                        cjl 99.4.27        */
/* Mult. scale by 4/3 since Mk4 has valid 0 rotator  rjc 99.8.3         */
/* Apply different normalization for 2 bit case      rjc 01.01.19       */
/* Break out normalization code into separate func.  rjc 01.11.7        */
/* apply delay offset by channel if non-zero         rjc 2011.8.10      */
/* allow multiple seq. or combined polarizations     rjc 2011.12.21     */
/* split into two routines for fx & xf correlators   rjc 2015.6.1       */
/*                                                                      */
/*      Inputs:     pass        Contains various essential parameters   */
/*                  fr          fourfit frequency channel number        */
/*                  ap          Sequential AP number                    */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <complex.h>
#include <fftw3.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

                                        /* minimum #bits for an AP to count */
#define SLIVER   0x1000
#define TWO_32   4294967296.0
#define SYSCLK_S 32.e6                  /* number of sysclks / sec */

void norm_xf (struct type_pass *pass, 
              int fr, 
              int ap)
    { 
    struct type_120 *t120;
    struct freq_corel *fdata;
    struct data_corel *datum;
    int flagbit, sb, st, i, rev_i, j, l, m, intshift, ct;
    static int nlags = 0;
    int cosbits, coscor, sinbits, sincor;
    int ip, ips;
    static complex xp_spec[4*MAXLAG];
    static complex xcor[4*MAXLAG], S[4*MAXLAG], xlag[4*MAXLAG];
    complex z;
    double c_mag ();
    float  fraction, count_frac;
    unsigned short bitshift;
    double theta, shift, dd, norm_const, fb_fact, fb_amp_ratio();
    double samp_per_ap, corrfact, rawcorr, factor, mean;
    double diff_delay, deltaf, polcof, polcof_sum, phase_shift;
    int freq_no,
        ibegin,
        sindex,
        pol,
        usb_present, lsb_present,
        usb_bypol[4],lsb_bypol[4];

    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L/X/H, 1:R/Y/V
    static fftw_plan fftplan_hw;
    static fftw_plan fftplan;

    extern struct type_param param;
    extern struct type_status status;

    if (pass->npols == 1)
        {
        pol = pass->pol;                // single pol being done per pass
        ips = pol;
        }
    else                                // linear combination of polarizations
        ips = 0;
        
                                        // do fft plan only iff nlags changes
    if (param.nlags != nlags)
        {
        nlags = param.nlags;
                                        // hw (lag) correlation requires extra fft plan
        fftplan_hw = fftw_plan_dft_1d (2 * nlags, xcor, xp_spec, FFTW_FORWARD, FFTW_MEASURE);

        fftplan = fftw_plan_dft_1d (4 * nlags, S, xlag, FFTW_FORWARD, FFTW_MEASURE);
        }
    samp_per_ap = param.acc_period / param.samp_period;
    freq_no = fcode(pass->pass_data[fr].freq_code);

                                        /* Point to current frequency */
    fdata = pass->pass_data + fr;
                                        /* Convenience pointer */
    datum = fdata->data + ap;
                                        /* Initialize */
    for (i = 0; i < nlags*4; i++)
        S[i] = 0.0;
        
    datum->sband = 0;
                                        /* -1.0 means no data, not zero weight */
    datum->usbfrac = -1.0;
    datum->lsbfrac = -1.0;

    polcof_sum = 0.0;

    usb_present = FALSE;
    lsb_present = FALSE;
                                        // check sidebands for each pol. for data
    for (ip=ips; ip<pass->pol+1; ip++)
        {
        usb_bypol[ip] = ((datum->flag & (USB_FLAG << 2*ip)) != 0);
        lsb_bypol[ip] = ((datum->flag & (LSB_FLAG << 2*ip)) != 0);

        usb_present |= usb_bypol[ip];
        lsb_present |= lsb_bypol[ip];
        }
    datum->sband = usb_present - lsb_present;
                                        /*  sideband # -->  0=upper , 1= lower */
    for (sb = 0; sb < 2; sb++) 
      {
      for (i = 0; i < nlags*4; i++)     // clear xcor & xp_spec for pol sum into them
          {
          xcor[i] = 0.0;
          xp_spec[i] = 0.0;
          }
      for (ip=ips; ip<pass->pol+1; ip++) // loop over polarization products
        {
        if (param.pol)
            pol = ip;
                                        // If no data for this sb/pol, go on to next
        if (sb == 0 & usb_bypol[ip] == 0
         || sb == 1 & lsb_bypol[ip] == 0)
            continue;
                                        /* Pluck out the requested polarization */
        switch (pol)
            {
            case POL_LL: t120 = datum->apdata_ll[sb];
                         polcof = (param.pol == POL_IXY) ?
                             cos (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            case POL_RR: t120 = datum->apdata_rr[sb];
                         polcof = (param.pol == POL_IXY) ?
                             cos (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            case POL_LR: t120 = datum->apdata_lr[sb];
                         polcof = (param.pol == POL_IXY) ?
                             sin (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            case POL_RL: t120 = datum->apdata_rl[sb];
                         polcof = (param.pol == POL_IXY) ?
                            -sin (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            }
        polcof_sum += polcof;
                                    /* Calculate normalization quantities */
        calc_normalization (sb, pol, datum, pass, &mean, &norm_const);
    
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
            if (t120 -> type == COUNTS_GLOBAL)
                {
                cosbits = t120->ld.cg.cosbits;
                coscor = t120->ld.cg.lags[i].coscor;
                sinbits = t120->ld.cg.sinbits;
                sincor = t120->ld.cg.lags[i].sincor;
                }
            else if (t120 -> type == COUNTS_PER_LAG)
                {
                cosbits = t120->ld.cpl[i].cosbits;
                coscor = t120->ld.cpl[i].coscor;
                sinbits = t120->ld.cpl[i].sinbits;
                sincor = t120->ld.cpl[i].sincor;
                }
            else if (t120 -> type == AUTO_GLOBAL)
                {                       /* autocorrelation lags are duplicated here,
                                       since 2 values of i map to the same m */
                if (i < nlags/2) m = nlags/2 - i;
                else m = i - nlags/2;
                                    /* Kluge to avoid zeros */
                                    // also avoid duplicate entries  rjc 2007.8.13
                if (i == 0)
                    continue;
                                    // old code: if (m == nlags/2) m -= 1;
                cosbits = t120->ld.ag.cosbits;
                coscor = t120->ld.ag.coscor[m];
                sinbits = 0;
                sincor = 0;
                }
            else if (t120 -> type == AUTO_PER_LAG)
                {
                if (i < nlags/2) m = nlags/2 - i;
                else m = i - nlags/2;
                                    /* Kluge to avoid zeros */
                                    // also avoid duplicate entries  rjc 2007.8.13
                if (i == 0)
                    continue;
                                    // old code: if (m == nlags/2) m -= 1;
                cosbits = t120->ld.apl[m].cosbits;
                coscor = t120->ld.apl[m].coscor;
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
                                    // mark this sideband & pol piece of this AP bad
                datum->flag &= ~(1 << (2 * pol + sb));
                break;              // break out of loop over lags
                }            
                                 
            if ((cosbits < SLIVER) || ((sinbits < SLIVER) && (! pass->autocorr)))
                {
                status.sliver_errors++;
                msg ("marking ap %d for a sliver error", 0, ap);
                                    // mark this sideband & pol piece of this AP bad
                datum->flag &= ~(1 << (2 * pol + sb));
                break;              /* break out of loop over lags  */
                }
                                    /* Convert lag values to true correlation
                                     * coefficients. Factor of 8 is due to hidden
                                     * 3 lower order bits in accumulator */
                                    /* Proper autocorrelation normalization is */
                                    /* lag-dependent, Need computation here */
            z = norm_const * (8.0 * coscor - mean * cosbits) / cosbits;
            if (pass->autocorr)
                ;                   // imaginary part is 0
                                    /* Conjugate if USB, not if LSB */
            else if (sb == 0)
                z = z - norm_const * (8.0 * sincor - mean * sinbits) / sinbits * I;
            else if (sb == 1)
                z = z + norm_const * (8.0 * sincor - mean * sinbits) / sinbits * I;

                                    /* Check for wild values, zero out and */
                                    /* flag for later accounting */
            if ((creal (z) > param.cor_limit) || (creal (z) < -param.cor_limit) 
                || (cimag (z) > param.cor_limit) || (cimag (z) < -param.cor_limit)) 
                {
                status.large_errors++;
                                    // mark this sideband & pol piece of this AP bad
                datum->flag &= ~(1 << (2 * pol + sb));
                break;              // break out of loop over lags
                }
                                    // apply pcal to data and add into xcor
                                    // iff this is a requested pol product
            if (param.pol & 1<<ip || param.pol == 0)
                {
                if (sb==0)
                    z = z * datum->pc_phasor[ip];
                else                // use conjugate of usb pcal tone for lsb
                    z = z * conj (datum->pc_phasor[ip]);

                z = z * polcof;

                xcor[l] += z;
                                    /* Accumulate data fractions */
                                    /* Makes use of fact that cosbits */
                                    /* = sinbits for cross-corr */
                if (sb) 
                    {
                    if (datum->lsbfrac < 0.0) 
                        datum->lsbfrac = 0.0;
                    datum->lsbfrac += (double) (cosbits)
                                    / (samp_per_ap * nlags);
                    }
                else 
                    {
                    if (datum->usbfrac < 0.0) 
                        datum->usbfrac = 0.0;
                    datum->usbfrac += (double) (cosbits)
                                    / (samp_per_ap * nlags);
                    }
                }
            }                       /* end of loop over i (lags) */
        
                                    /* In case large error zapped this datum ... */
        if ((datum->flag & (1 << (2 * pol + sb))) == 0)
            {
            msg ("skipping data for ap %d pol %d sb %d", -2, ap, pol, sb);
            continue;               /* skip over rest of the sideband loop */
            }
                                    // only do frequency domain calculations once
        if (ip != pass->pol)        // so check for last polarization in loop
            continue;
                                    // bump accumulation period counters
        status.ap_num[sb][fr]++;
        status.total_ap++;
                                    /* Microedited counts */
        if (sb) 
            {
            status.ap_frac[sb][fr] += datum->lsbfrac;
            status.total_ap_frac   += datum->lsbfrac;
            status.total_lsb_frac  += datum->lsbfrac;
            }
        else 
            {
            status.ap_frac[sb][fr] += datum->usbfrac;
            status.total_ap_frac   += datum->usbfrac;
            status.total_usb_frac  += datum->usbfrac;
            }
                                    /* Avg to find center of scan */
        status.epoch_off_cent += (double)ap;  
                                    /* FFT to X-power spectrum  */
        fftw_execute (fftplan_hw);

                                    /* Adjust for Bit-shift */
                                    /* First, decode total bitshift shift */
        shift = fabs (t120->delay_rate / TWO_32 * param.acc_period * SYSCLK_S);
                                    /* Get fractional part */
        if (param.bocfs_per_ap % 2) // if odd # of cf's/AP must 
                                    // extrapolate fractional delay to cf center
            {
            fraction = (t120->fr_delay + 
                        0.5 * param.bocf_period * t120->delay_rate) / TWO_32;
                                    /* adjust fraction to lie in [-0.5, 0.5] */
            if (fraction > 0.5)
                fraction -= 1.0;
            
            if (fraction < -0.5)
                fraction += 1.0;
            }
        else
                                    /* even # of corr. frames per AP */
            fraction = t120->fr_delay / TWO_32;

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
                                    /* Convert to angle, avoid zero division */
        if (shift == 0.0) 
            shift = 1.0;

                                    /* switched sign of theta  rjc 99.11.19 */
        theta = - M_PI / nlags * (fraction - dd) * (1 - intshift / shift);

                                    // retroactive fix for non-zero values of delay
                                    // and delay_rate coming from correlator in the
                                    // case of an autocorrelation      rjc 2007.8.14
        if (t120->type == AUTO_GLOBAL || t120->type == AUTO_PER_LAG)
            {
            fraction = 0.0;
            shift = 1.0;
            theta = 0.0;
            }
                                    /* Factor to correct AP amplitudes by */
                                    /* Maximum of +- 0.2% */
        fb_fact = fb_amp_ratio ((double) fraction, shift);
        }                           // bottom of polarization loop

                                    // also skip over this next section, if no data
      if (sb == 0 & usb_present == 0
       || sb == 1 & lsb_present == 0)
          continue;

                                    /* apply spectral filter as needed */
      apply_passband (sb, fdata, xp_spec, nlags*2);

                                    /* Put sidebands together.  For each sb,
                                       the Xpower array, which is the FFT across
                                       lags, is symmetrical about DC of the
                                       sampled sideband, and thus contains the
                                       (filtered out) "other" sideband, which
                                       consists primarily of noise.  Thus we only
                                       copy in half of the Xpower array
                                       Weight each sideband by data fraction */

                                    // skip 0th spectral pt if DC channel suppressed
      ibegin = (pass->control.dc_block) ? 1 : 0;
      if (sb == 0)
          { 
          for (i = ibegin; i < nlags; i++)
              {
              factor = fb_fact * datum->usbfrac;
              S[i] += factor * xp_spec[i] * cexp (I * theta * (double)(i-nlags/2));
              }
          }
      else                          /* LSB: conjugate, add phase offset */
          {
          for (i = ibegin; i < nlags; i++)
              {
              factor = fb_fact * datum->lsbfrac;
                                    // DC+highest goes into middle element of the S array
              sindex = i ? 4 * nlags - i : 2 * nlags;
              S[sindex] += factor * conj (xp_spec[i] * 
                  cexp (I * (theta * (double)(i-nlags/2) + status.lsb_phoff[0] - status.lsb_phoff[1])));
              }
          }
      }                             // bottom of sideband loop 

                                    /* Normalize data fractions
                                       The resulting sbdelay functions which
                                       are attached to each AP from this point
                                       on reflect twice as much power in the
                                       double sideband case as in single sideband.
                                       The usbfrac and lsbfrac numbers determine
                                       a multiplicative weighting factor to be
                                       applied.  In the double sideband case, the
                                       factor of two is inherent in the data values
                                       and additional weighting should be done
                                       using the mean of usbfrac and lsbfrac */
    factor = 0.0;
    if (datum->usbfrac >= 0.0)
        factor += datum->usbfrac;
    if (datum->lsbfrac >= 0.0)
        factor += datum->lsbfrac;
    if ((datum->usbfrac >= 0.0) && (datum->lsbfrac >= 0.0)) 
        factor /= 4.0;              // x2 factor for sb and for polcof
                                    // correct for multiple pols being added in
    factor *= polcof_sum;
    if (factor > 0.0)
        factor = 1.0 / factor;
    msg ("usbfrac %f lsbfrac %f polcof_sum %f factor %1f", -2, 
            datum->usbfrac, datum->lsbfrac, polcof_sum, factor);
    for (i=0; i<4*nlags; i++) 
        S[i] = S[i] * factor;

                                    /* Collect the results */
    if(datum->flag != 0)
        {
                                    // corrections to phase as fn of freq based upon 
                                    // delay calibrations
                                    // apply delay offset phase ramp
                                // add in phase effects if multitone delays 
                                // were extracted
        if (pass->control.nsamplers && param.pc_mode[0] == MULTITONE 
                                    && param.pc_mode[1] == MULTITONE)      
            diff_delay = -1e9 * (datum->rem_sdata.mt_delay[stnpol[1][pol]] 
                              - datum->ref_sdata.mt_delay[stnpol[0][pol]]);
        else
            diff_delay = pass->control.delay_offs[freq_no].rem 
                       - pass->control.delay_offs[freq_no].ref;

        if (diff_delay != 0.0)  // apply only non-zero delay effects
            {
            msg ("ap %d fr %d diff_delay %f", -2, ap, fr, diff_delay);
            for (i=1, j=4*nlags-1; i<nlags; i++, j--)
                {
                                // calculate offset frequency of DC edge in
                                // GHz from the reference frequency for USB
                deltaf = 1e-3 * i / (2e6 * param.samp_period * nlags);
                if (datum->sband)       // refer phase ramp to actual midband
                    deltaf -= 1e-3 * datum->sband / (4e6 * param.samp_period);
                                // apply phase ramp to spectral points 
                S[i] = S[i] * cexp (-2.0 * M_PI * I * diff_delay * deltaf);
                                // ditto for LSB
                S[j] = S[j] * cexp (2.0 * M_PI * I * diff_delay * deltaf);
                }
            }
                                    /* FFT to single-band delay */
        fftw_execute (fftplan);
                                    /* Place SB delay values in data structure */
        for (i = 0; i < 2*nlags; i++)
            {
                                /* Translate so i=nlags is central lag */
            j = i - nlags;
            if (j < 0) 
                j += 4 * nlags;
                                /* re-normalize back to single lag */
                                /* (property of FFTs) */
                                // nlags-1 norm. iff zeroed-out DC
            if (pass->control.dc_block)
                datum->sbdelay[i] = xlag[j] / (nlags - 1.0);
            else
                datum->sbdelay[i] = xlag[j] / nlags;
            }
         }
                                    /* No data */
    else
        for (i = 0; i < nlags*2; i++) 
            datum->sbdelay[i] = 0.0;
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

    f2 = f * f;                     // pre-calculate useful factors
    r2 = r * r;

    fp1 = (int) floor (f + r / 2 + 0.5);
    fp2 = fp1 * fp1;                // named by <floor><sign><power>
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
