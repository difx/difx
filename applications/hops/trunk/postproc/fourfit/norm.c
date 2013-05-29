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
/*                                                                      */
/*      Inputs:     pass        Contains various essential parameters   */
/*                  fr          fourfit frequency channel number        */
/*                  ap          Sequential AP number                    */
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

norm (struct type_pass *pass, 
      int fr, 
      int ap)
    { 
    struct type_120 *rawdatum;
    struct freq_corel *fdata;
    struct data_corel *datum;
    int flagbit, sb, st, i, rev_i, j, l, m, intshift, ct, nlags;
    int cosbits, coscor, sinbits, sincor;
    int ip, ips;
    complex xcor[4*MAXLAG], xp_spec[2*MAXLAG], S[4*MAXLAG];
    complex c_zero(), conju(), c_mult(), c_add(), c_exp(), s_mult();
    complex z;
    double c_mag ();
    float  fraction, count_frac;
    unsigned short bitshift;
    double theta, shift, dd, norm_const, fb_fact, fb_amp_ratio();
    double samp_per_ap, corrfact, rawcorr, factor, mean;
    double diff_delay, deltaf, polcof, polcof_sum;
    int freq_no,
        ibegin,
        sindex,
        pol,
        usb_present, lsb_present;

    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L/X/H, 1:R/Y/V
    extern struct type_param param;
    extern struct type_status status;

    if (pass->npols == 1)
        {
        pol = pass->pol;                // single pol being done per pass
        ips = pol;
        }
    else                                // linear combination of polarizations
        ips = 0;
        
    nlags = param.nlags;
    samp_per_ap = param.acc_period / param.samp_period;
    freq_no = fcode(pass->pass_data[fr].freq_code);

                                        /* Point to current frequency */
    fdata = pass->pass_data + fr;
                                        /* Convenience pointer */
    datum = fdata->data + ap;
                                        /* Initialize */
    for (i = 0; i < nlags*4; i++)
        S[i] = c_zero();
        
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
        usb_present |= ((datum->flag & (USB_FLAG << 2*ip)) != 0);
        lsb_present |= ((datum->flag & (LSB_FLAG << 2*ip)) != 0);
        }
    datum->sband = usb_present - lsb_present;
                                        /*  sideband # -->  0=upper , 1= lower */
    for (sb = 0; sb < 2; sb++) 
      {
      for (i = 0; i < nlags*4; i++)     // clear xcor & xp_spec for pol sum into them
          {
          xcor[i]=c_zero();
          xp_spec[i] = c_zero();
          }
      for (ip=ips; ip<pass->pol+1; ip++) // loop over polarization products
        {
        if (param.pol)
            pol = ip;
                                        // If no data for this sb/pol, go on to next
        if (sb == 0 & usb_present == 0
         || sb == 1 & lsb_present == 0)
            continue;
                                        /* Pluck out the requested polarization */
        switch (pol)
            {
            case POL_LL: rawdatum = datum->apdata_ll[sb];
                         polcof = (param.pol == POL_IXY) ?
                             cos (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            case POL_RR: rawdatum = datum->apdata_rr[sb];
                         polcof = (param.pol == POL_IXY) ?
                             cos (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            case POL_LR: rawdatum = datum->apdata_lr[sb];
                         polcof = (param.pol == POL_IXY) ?
                             sin (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            case POL_RL: rawdatum = datum->apdata_rl[sb];
                         polcof = (param.pol == POL_IXY) ?
                            -sin (param.par_angle[1] - param.par_angle[0]) :
                             1.0;
                         break;
            }
        polcof_sum += polcof;
        if (param.corr_type == MK4HDW)
            {
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
                z.re = norm_const * (8.0 * coscor - mean * cosbits) / cosbits;
                if (pass->autocorr)
                    z.im = 0.0;
                                        /* Conjugate if USB, not if LSB */
                else if (sb == 0)
                    z.im = -norm_const * (8.0 * sincor - mean * sinbits) / sinbits;
                else if (sb == 1)
                    z.im =  norm_const * (8.0 * sincor - mean * sinbits) / sinbits;

                                        /* Check for wild values, zero out and */
                                        /* flag for later accounting */
                if ((z.re > param.cor_limit) || (z.re < -param.cor_limit) 
                    || (z.im > param.cor_limit) || (z.im < -param.cor_limit)) 
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
                        z = c_mult (z, datum->pc_phasor[ip]);
                    else                // use conjugate of usb pcal tone for lsb
                        z = c_mult (z, conju (datum->pc_phasor[ip]));

                    z = s_mult (z, polcof);

                    xcor[l].re += z.re;
                    xcor[l].im += z.im;
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
                }
            else 
                {
                status.ap_frac[sb][fr] += datum->usbfrac;
                status.total_ap_frac   += datum->usbfrac;
                }
                                        /* Avg to find center of scan */
            status.epoch_off_cent += (double)ap;  
                                        /* FFT to X-power spectrum  */
            FFT1 (xcor, nlags*2, 1, xp_spec, 1);

                                        /* apply spectral filter as needed */
            apply_passband (sb, fdata, xp_spec, nlags*2);

                                        /* Adjust for Bit-shift */
                                        /* First, decode total bitshift shift */
            shift = fabs (rawdatum->delay_rate / TWO_32 * param.acc_period * SYSCLK_S);
                                        /* Get fractional part */
            if (param.bocfs_per_ap % 2) // if odd # of cf's/AP must 
                                        // extrapolate fractional delay to cf center
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
                                        /* Convert to angle, avoid zero division */
            if (shift == 0.0) 
                shift = 1.0;

                                        /* switched sign of theta  rjc 99.11.19 */
            theta = - M_PI / nlags * (fraction - dd) * (1 - intshift / shift);

                                        // retroactive fix for non-zero values of delay
                                        // and delay_rate coming from correlator in the
                                        // case of an autocorrelation      rjc 2007.8.14
            if (rawdatum->type == AUTO_GLOBAL || rawdatum->type == AUTO_PER_LAG)
                {
                fraction = 0.0;
                shift = 1.0;
                theta = 0.0;
                }
                                        /* Factor to correct AP amplitudes by */
                                        /* Maximum of +- 0.2% */
            fb_fact = fb_amp_ratio ((double) fraction, shift);
            }
        else                            // difx (spectral) mode; just copy spectrum in
            {
            if (rawdatum -> type != SPECTRAL)
                {
                msg ("Conflicting correlation type %d", 2, rawdatum->type);
                return;
                }
            
            if (ip == pass->pol)
                {                   // last included polarization, do totals
                status.ap_num[sb][fr]++;
                status.total_ap++;
                                    // sum to micro-edited totals
                if (sb) 
                    {
                    datum->lsbfrac = 1.0;
                    status.ap_frac[sb][fr] += datum->lsbfrac;
                    status.total_ap_frac   += datum->lsbfrac;
                    }
                else 
                    {
                    datum->usbfrac = 1.0;
                    status.ap_frac[sb][fr] += datum->usbfrac;
                    status.total_ap_frac   += datum->usbfrac;
                    }
                }
            fb_fact = 1.0;          // may want to re-examine this assumption
            theta = 0.0;            // no fractional bit induced error correction

            diff_delay = pass->control.delay_offs[freq_no].rem 
                       - pass->control.delay_offs[freq_no].ref;
                                    // add in phase effects if multitone delays 
                                    // were extracted
            if (pass->control.nsamplers && param.pc_mode[0] == MULTITONE 
                                        && param.pc_mode[1] == MULTITONE)      
                diff_delay += 1e9 * (datum->rem_sdata.mt_delay[stnpol[1][pol]] 
                                   - datum->ref_sdata.mt_delay[stnpol[0][pol]]);
            msg ("ap %d fr %d pol %d diff_delay %f", -2, ap, fr, pol, diff_delay);
                                    // loop over spectral points
            for (i=0; i<nlags; i++)
                                    // filter out any nan's, if present
                if (isnan (rawdatum->ld.spec[i].re) || isnan (rawdatum->ld.spec[i].im))
                    msg ("omitting nan's in visibility for ap %d fr %d lag %i", 
                          2, ap, fr, i);
                     
                                    // add in iff this is a requested pol product
                else if (param.pol & 1<<ip || param.pol == 0)
                    {           
                    z.re = rawdatum->ld.spec[i].re;
                    z.im = rawdatum->ld.spec[i].im;
                                    // rotate each pol prod by pcal prior to adding in
                    if (sb==0)
                        z = c_mult (z, datum->pc_phasor[ip]);
                    else            // use conjugate of usb pcal tone for lsb
                        z = c_mult (z, conju (datum->pc_phasor[ip]));
                                    // scale phasor by polarization coefficient
                    z = s_mult (z, polcof);
                                    // corrections to phase as fn of freq based upon 
                                    // delay calibrations

                                    // calculate offset frequency in GHz 
                                    // from DC edge for this spectral point
                    deltaf = 1e-3 * i / (2e6 * param.samp_period * nlags);
                                    // refer phase ramp to actual midband
                    if (datum->sband)
                        deltaf -= 1e-3 / (4e6 * param.samp_period);
                                    // apply phase ramp to spectral points 
                    z = c_mult (z, c_exp (-2.0 * M_PI * diff_delay * deltaf));
                    xp_spec[i].re += z.re;
                    xp_spec[i].im += z.im;
                    }
            }                       // end of spectral mode code
        }                           // bottom of polarization loop

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
              S[i] = s_mult (c_mult (xp_spec[i], 
                      c_exp (theta * (double)(i-nlags/2))),factor);
              }
          }
      else                          /* LSB: conjugate, add phase offset */
          {
          for (i = ibegin; i < nlags; i++)
              {
              factor = fb_fact * datum->lsbfrac;
                                    // DC goes into 1st element of the S array
              sindex = i ? 4 * nlags - i : 0;
              S[sindex] = s_mult (conju (c_mult (xp_spec[i], 
                             c_exp (theta * (double)(i-nlags/2) 
                           + status.lsb_phoff[0] - status.lsb_phoff[1]))),factor);
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
        S[i] = s_mult (S[i], factor);

                                    /* Collect the results */
    if(datum->flag != 0)
        {
                                    // corrections to phase as fn of freq based upon 
                                    // delay calibrations
                                    // apply delay offset phase ramp
                                    // spectral version is done above
        if (param.corr_type == MK4HDW)
            {
            diff_delay = pass->control.delay_offs[freq_no].rem 
                       - pass->control.delay_offs[freq_no].ref;
                                    // add in phase effects if multitone delays 
                                    // were extracted
            if (pass->control.nsamplers && param.pc_mode[0] == MULTITONE 
                                        && param.pc_mode[1] == MULTITONE)      
                diff_delay += 1e9 * (datum->rem_sdata.mt_delay[stnpol[1][pol]] 
                                   - datum->ref_sdata.mt_delay[stnpol[0][pol]]);

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
                    S[i] = c_mult (S[i], c_exp (-2.0 * M_PI * diff_delay * deltaf));
                                    // ditto for LSB
                    S[j] = c_mult (S[j], c_exp (2.0 * M_PI * diff_delay * deltaf));
                    }
                }
            }
                                    /* FFT to single-band delay */
        FFT1 (S, nlags*4, 1, xcor, 1);
                                    /* Place SB delay values in data structure */
        for (i = 0; i < nlags*2; i++)
            {
                                    /* Translate so i=nlags is central lag */
            j = i - nlags;
            if (j < 0) 
                j += nlags*4;
                                    /* re-normalize back to single lag */
                                    /* (property of FFTs) */
                                    // nlags-1 norm. iff zeroed-out DC
            if (pass->control.dc_block)
                datum->sbdelay[i] = s_mult (xcor[j], 1.0 / (nlags - 1.0));
            else
                datum->sbdelay[i] = s_mult (xcor[j], 1.0 / nlags);
            }
         }
                                    /* No data */
    else
        for (i = 0; i < nlags*2; i++) 
            datum->sbdelay[i] = c_zero();
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
