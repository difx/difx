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

#define signum(a) (a>=0 ? 1.0 : -1.0)

#define CIRC_MODE 0
#define LIN_MODE 1
#define MIXED_MODE 2

void norm_fx (struct type_pass *pass, 
              int fr, 
              int ap)
    { 
    struct type_120 *t120;
    struct freq_corel *fdata;
    struct data_corel *datum;
    int sb, st, i, rev_i, j, l, m;
    static int nlags = 0;
    int ip, ips;
    static complex xp_spec[4*MAXLAG];
    static complex xcor[4*MAXLAG], S[4*MAXLAG], xlag[4*MAXLAG];
    complex z;
    double factor, mean;
    double diff_delay, deltaf, polcof, polcof_sum, phase_shift, dpar;
    int freq_no,
        ibegin,
        sindex,
        pol,
        pols,                       // bit-mapped pols to be processed in this pass
        usb_present, lsb_present,
        usb_bypol[4],lsb_bypol[4], 
        lastpol[2];                 // last pol index with data present, by sideband

    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L/X/H, 1:R/Y/V
    static fftw_plan fftplan;

    extern struct type_param param;
    extern struct type_status status;

    //determine if the ref and rem stations are using circular or linear
    //feeds, or it is some combination
    int station_pol_mode = CIRC_MODE;
    if( pass->linpol[0] == 0 && pass->linpol[1] == 0){station_pol_mode = CIRC_MODE;}
    if( pass->linpol[0] == 1 && pass->linpol[1] == 1){station_pol_mode = LIN_MODE;}
    if( pass->linpol[0] != pass->linpol[1] ){station_pol_mode = MIXED_MODE;}

    if (pass->npols == 1)
        {
        pol = pass->pol;            // single pol being done per pass
        ips = pol;
        pols = 1 << pol;
        }
    else                            // linear combination of polarizations
        {
        ips = 0;
        pols = param.pol;
        }
        
                                    // do fft plan only iff nlags changes
    if (param.nlags != nlags)
        {
        nlags = param.nlags;
        fftplan = fftw_plan_dft_1d (4 * nlags, S, xlag, FFTW_FORWARD, FFTW_MEASURE);
        }
    freq_no = fcode(pass->pass_data[fr].freq_code);

                                    /* Point to current frequency */
    fdata = pass->pass_data + fr;
                                    /* Convenience pointer */
    datum = fdata->data + ap;
                                    // differenced parallactic angle
    dpar = param.par_angle[1] - param.par_angle[0];
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
    lastpol[0] = ips;
    lastpol[1] = ips;
                                    // check sidebands for each pol. for data
    for (ip=ips; ip<pass->pol+1; ip++)
        {
        usb_bypol[ip] = ((datum->flag & (USB_FLAG << 2*ip)) != 0)
                     && ((pols & (1 << ip)) != 0);
        lsb_bypol[ip] = ((datum->flag & (LSB_FLAG << 2*ip)) != 0)
                     && ((pols & (1 << ip)) != 0);
        pass->pprods_present[ip] |= usb_bypol[ip] || lsb_bypol[ip];

        if (usb_bypol[ip])
            lastpol[0] = ip;
        if (lsb_bypol[ip])
            lastpol[1] = ip;

        usb_present |= usb_bypol[ip];
        lsb_present |= lsb_bypol[ip];
        }
    datum->sband = usb_present - lsb_present;
                                    /*  sideband # -->  0=upper , 1= lower */
    for (sb = 0; sb < 2; sb++) 
      {
      for (i = 0; i < nlags*4; i++) // clear xcor & xp_spec for pol sum into them
          {
          xcor[i] = 0.0;
          xp_spec[i] = 0.0;
          }
                                    // loop over polarization products
      for (ip=ips; ip<pass->pol+1; ip++)
        {
        if (param.pol)
            pol = ip;
                                    // If no data for this sb/pol, go on to next
        if (sb == 0 & usb_bypol[ip] == 0
         || sb == 1 & lsb_bypol[ip] == 0)
            continue;
                                    // Pluck out the requested polarization
        switch (pol)
            {
            case POL_LL: t120 = datum->apdata_ll[sb];
            if(station_pol_mode == LIN_MODE)  //TODO: check if this correction should also be applied in mixed-mode case
            {
                         polcof = (pass->npols > 1) ?
                             cos (dpar) :
                             signum (cos (dpar));
            }
            else
            {
                polcof = 1;
            }
                         break;
            case POL_RR: t120 = datum->apdata_rr[sb];
            if(station_pol_mode == LIN_MODE)
            {
                         polcof = (pass->npols > 1) ?
                             cos (dpar) :
                             signum (cos (dpar));
            }
            else
            {
                polcof = 1;
            }
                         break;
            case POL_LR: t120 = datum->apdata_lr[sb];
            if(station_pol_mode == LIN_MODE)
            {
                         polcof = (pass->npols > 1) ?
                             sin (-dpar) :
                             signum (sin (-dpar));
            }
            else
            {
                polcof = 1;
            }
                         break;
            case POL_RL: t120 = datum->apdata_rl[sb];
            if(station_pol_mode == LIN_MODE)
            {
                         polcof = (pass->npols > 1) ?
                             sin (dpar) :
                             signum (sin (dpar));
            }
            else
            {
                polcof = 1;
            }
                         break;
            }
        polcof_sum += fabs (polcof);
                                    // sanity test
        if (t120 -> type != SPECTRAL)
            {
            msg ("Conflicting correlation type %d", 2, t120->type);
            return;
            }
        
                                    // determine data weights by sideband
        if (ip == lastpol[sb])
            {                       // last included polarization, do totals
            status.ap_num[sb][fr]++;
            status.total_ap++;
                                    // sum to micro-edited totals
            if (sb)                 // lower sideband
                {                   // 0 weight encoded by negative 0
                if (*((unsigned int *)(&(t120->fw.weight))) == 0)
                                    // +0 is backward-compatibility for no weight
                    datum->lsbfrac = 1.0;
                else
                    datum->lsbfrac = t120->fw.weight;
                status.ap_frac[sb][fr] += datum->lsbfrac;
                status.total_ap_frac   += datum->lsbfrac;
                status.total_lsb_frac   += datum->lsbfrac;
                }
            else                    // upper sideband
                {
                if (*((unsigned int *)(&(t120->fw.weight))) == 0)
                    datum->usbfrac = 1.0;
                else
                    datum->usbfrac = t120->fw.weight;
                status.ap_frac[sb][fr] += datum->usbfrac;
                status.total_ap_frac   += datum->usbfrac;
                status.total_usb_frac   += datum->usbfrac;
                }
            }

                                    // add in phase effects if multitone delays 
                                    // were extracted
        if (pass->control.nsamplers && param.pc_mode[0] == MULTITONE 
                                    && param.pc_mode[1] == MULTITONE)      
            diff_delay = +1e9 * (datum->rem_sdata.mt_delay[stnpol[1][pol]]
                               - datum->ref_sdata.mt_delay[stnpol[0][pol]]);
                                    // ##DELAY_OFFS## otherwise assume user has
                                    // used delay_offs or delay_offs_? but not both.
        else
            diff_delay = pass->control.delay_offs_pol[freq_no][stnpol[1][pol]].rem
                       + pass->control.delay_offs[freq_no].rem  // ##DELAY_OFFS##
                       - pass->control.delay_offs[freq_no].ref  // ##DELAY_OFFS##
                       - pass->control.delay_offs_pol[freq_no][stnpol[0][pol]].ref;
        msg ("ap %d fr %d pol %d diff_delay %f", -2, ap, fr, pol, diff_delay);

                                    // loop over spectral points
        for (i=0; i<nlags/2; i++)
                                    // filter out any nan's, if present
            if (isnan (t120->ld.spec[i].re) || isnan (t120->ld.spec[i].im))
                msg ("omitting nan's in visibility for ap %d fr %d lag %i", 
                      2, ap, fr, i);
                 
                                    // add in iff this is a requested pol product
            else if (param.pol & 1<<ip || param.pol == 0)
                {           
                z = t120->ld.spec[i].re + I * t120->ld.spec[i].im;
                                    // rotate each pol prod by pcal prior to adding in
                if (sb==0)
                    z = z * datum->pc_phasor[ip];
                else                // use conjugate of usb pcal tone for lsb
                    z = z * conj (datum->pc_phasor[ip]);
                                    // scale phasor by polarization coefficient
                z = z * polcof;
                                    // corrections to phase as fn of freq based upon 
                                    // delay calibrations

                                    // calculate offset frequency in GHz 
                                    // from DC edge for this spectral point
                deltaf = -2e-3 * i / (2e6 * param.samp_period * nlags);

                                    // correction to mean phase depends on sideband
                phase_shift = - 1e-3 * diff_delay / (4e6 * param.samp_period);
                if (sb)
                    phase_shift = -phase_shift;

                                    // apply phase ramp to spectral points 
                z = z * cexp (-2.0 * M_PI * I * (diff_delay * deltaf + phase_shift));
                xp_spec[i] += z;
                }
        }                           // bottom of polarization loop

                                    // also skip over this next section, if no data
      if (sb == 0 && usb_present == 0
       || sb == 1 && lsb_present == 0)
          continue;

                                    /* apply spectral filter as needed */
      apply_passband (sb, ap, fdata, xp_spec, nlags*2, datum);
      apply_notches (sb, ap, fdata, xp_spec, nlags*2, datum);

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
              factor = datum->usbfrac;
              S[i] += factor * xp_spec[i];
              }
          }
      else                          /* LSB: conjugate, add phase offset */
          {
          for (i = ibegin; i < nlags; i++)
              {
              factor = datum->lsbfrac;
                                    // DC+highest goes into middle element of the S array
              sindex = i ? 4 * nlags - i : 2 * nlags;
              S[sindex] += factor * conj (xp_spec[i] * 
                  cexp (I * (status.lsb_phoff[0] - status.lsb_phoff[1])));
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
                                    /* FFT to single-band delay */
        fftw_execute (fftplan);
                                    /* Place SB delay values in data structure */
                                    // FX correlator - use full xlag range
        for (i = 0; i < 2*nlags; i++)
            {
                                    /* Translate so i=nlags is central lag */
                                    // skip every other (interpolated) lag
            j = 2 * (i - nlags);
            if (j < 0) 
                j += 4 * nlags;
                                    /* re-normalize back to single lag */
                                    /* (property of FFTs) */
                                    // nlags-1 norm. iff zeroed-out DC
                                    // factor of 2 for skipped lags
            if (pass->control.dc_block)
                datum->sbdelay[i] = xlag[j] / (nlags / 2 - 1.0);
            else
                datum->sbdelay[i] = xlag[j] / (nlags / 2);
            }
         }
                                    /* No data */
    else
        for (i = 0; i < nlags*2; i++) 
            datum->sbdelay[i] = 0.0;
    }
