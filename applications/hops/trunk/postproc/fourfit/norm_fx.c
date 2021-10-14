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
#include "hops_complex.h"
#include <fftw3.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "adhoc_flag.h"
#include "apply_funcs.h"
#include "ff_misc_if.h"

#define signum(a) (a>=0 ? 1.0 : -1.0)

#define CIRC_MODE 0
#define LIN_MODE 1
#define MIXED_MODE 2
#define CIRC_PAREL 3

void norm_fx (struct type_pass *pass,
              struct type_param *param,
              struct type_status *status,
              int fr, 
              int ap)
    { 
    struct type_120 *t120;
    struct freq_corel *fdata;
    struct data_corel *datum;
    int sb, st, i, rev_i, j, l, m;
    static int nlags = 0;
    int ip, ips;
    static hops_complex xp_spec[4*MAXLAG];
    static hops_complex xcor[4*MAXLAG], S[4*MAXLAG], xlag[4*MAXLAG];
    hops_complex z;
    double factor, mean, tmp_apfrac;
    double diff_delay, deltaf, polcof, polcof_sum, phase_shift, dpar;
    int freq_no,
        ibegin,
        sindex,
        pol,
        pols,                       // bit-mapped pols to be processed in this pass
        usb_present, lsb_present,
        usb_bypol[4],lsb_bypol[4], 
        lastpol[2];                 // last pol index with data present, by sideband
    int datum_uflag, datum_lflag;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L/X/H, 1:R/Y/V
    static fftw_plan fftplan;
    hops_complex cpolrotfac = 1.0;      // default for non CIRC_PAREL
    hops_complex cpolvalue[4];          // compute these once
    int station_pol_mode = CIRC_MODE;   // see assignment below

    //determine if the ref and rem stations are using circular or linear
    //feeds, or it is some combination
    if( pass->linpol[0] == 0 && pass->linpol[1] == 0){station_pol_mode = CIRC_MODE;}
    if( pass->linpol[0] == 1 && pass->linpol[1] == 1){station_pol_mode = LIN_MODE;}
    if( pass->linpol[0] != pass->linpol[1] ){station_pol_mode = MIXED_MODE;};

    // it is NOT clear how the preceding logic works with POL_IXY
    // see parallactic-angle-correction.txt for discussion; in any
    // case these calculations should be done once outside the loops
    if (station_pol_mode == CIRC_MODE &&
        (param->mount_type[0] != NO_MOUNT_TYPE ||
         param->mount_type[1] != NO_MOUNT_TYPE))
            {
            station_pol_mode = CIRC_PAREL;
            compute_field_rotations_fixed(cpolvalue, param->par_angle,
                param->elevation, param->mount_type);
            // NYI:
            // compute_field_rotations_byap(cpolvalue, parangle, elev, mnt, ap);
            }

    if (pass->npols == 1)
        {
        pol = pass->pol;            // single pol being done per pass
        ips = pol;                  // param->pol == 0 for this case
        pols = 1 << pol;            // mask with the one bit set
        }
    else                            // linear combination of polarizations
        {
        // pol not set
        ips = 0;
        pols = param->pol;          // mask of pols in combination
        }
        
                                    // do fft plan only iff nlags changes
    if (param->nlags != nlags)
        {
        nlags = param->nlags;
        fftplan = fftw_plan_dft_1d (4 * nlags, S, xlag, FFTW_FORWARD, FFTW_MEASURE);
        }
    freq_no = fcode(pass->pass_data[fr].freq_code, pass->control.chid);

                                    /* Point to current frequency */
    fdata = pass->pass_data + fr;
                                    /* Convenience pointer */
    datum = fdata->data + ap;
                                    // differenced parallactic angle
    dpar = param->par_angle[1] - param->par_angle[0];

                                        /* Initialize */
    for (i = 0; i < nlags*4; i++)
        S[i] = 0.0;
        
    datum->sband = 0;
                                    /* -1.0 means no data, not zero weight */
    datum->usbfrac = -1.0;
    datum->lsbfrac = -1.0;

    polcof_sum = 0.0;

    // TRUE(1) and FALSE(0) are defined in mk4_sizes.h imported from mk4_data.h
    usb_present = FALSE;
    lsb_present = FALSE;
    lastpol[0] = ips;
    lastpol[1] = ips;
                                    // check sidebands for each pol. for data
    ADHOC_FLAG(param, datum->flag, fr, ap, &datum_uflag, &datum_lflag);
    for (ip=ips; ip<pass->pol+1; ip++)
        {
        usb_bypol[ip] = ((datum_uflag & (USB_FLAG << 2*ip)) != 0)
                     && ((pols & (1 << ip)) != 0);
        lsb_bypol[ip] = ((datum_lflag & (LSB_FLAG << 2*ip)) != 0)
                     && ((pols & (1 << ip)) != 0);
        pass->pprods_present[ip] |= usb_bypol[ip] || lsb_bypol[ip];

        if (usb_bypol[ip])
            lastpol[0] = ip;
        if (lsb_bypol[ip])
            lastpol[1] = ip;

        usb_present |= usb_bypol[ip];
        lsb_present |= lsb_bypol[ip];
        }
    //datum->sband is 1 (usb only), 0(both or neither) or -1 (lsb only).
    //it is only used in vrot
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
        if (param->pol)             // query pol mask and set pol to be the POL_XX type
            pol = ip;               // otherwise pol remains as set above, i.e. pass->pol
        else
            pol = pass->pol;        // replicating setting ab initio
                                    // If no data for this sb/pol, go on to next
        if ((sb == 0 && usb_bypol[ip] == 0)
         || (sb == 1 && lsb_bypol[ip] == 0))
            continue;

        // The following block does a parallactic angle correction in LIN_MODE
        // Check if this correction should also be applied in mixed-mode case.

        // However, the above comment was never followed-up, and it is not clear
        // how useful this LIN_MODE method ultimately was.  Note that this business
        // of adding polarizations is spawed from parse_cmdline:parse_polar which
        // allows a general addition of up to 4 polarization types.

                                    // Pluck out the requested polarization
        switch (pol)
            {
            case POL_LL: t120 = datum->apdata_ll[sb];
                if(station_pol_mode == LIN_MODE)
                    {
                    polcof = (pass->npols > 1) ? cos (dpar) : signum (cos (dpar));
                    }
                else if (station_pol_mode == CIRC_PAREL)
                    {
                    polcof = 1;
                    cpolrotfac = cpolvalue[pol];
                    }
                else
                    {
                    polcof = 1;
                    }
                break;
            case POL_RR: t120 = datum->apdata_rr[sb];
                if(station_pol_mode == LIN_MODE)
                    {
                    polcof = (pass->npols > 1) ? cos (dpar) : signum (cos (dpar));
                    }
                else if (station_pol_mode == CIRC_PAREL)
                    {
                    polcof = 1;
                    cpolrotfac = cpolvalue[pol];
                    }
                else
                    {
                    polcof = 1;
                    }
                break;
            case POL_LR: t120 = datum->apdata_lr[sb];
                if(station_pol_mode == LIN_MODE)
                    {
                    polcof = (pass->npols > 1) ? sin (-dpar) : signum (sin (-dpar));
                    }
                else if (station_pol_mode == CIRC_PAREL)
                    {
                    polcof = 1;
                    cpolrotfac = cpolvalue[pol];
                    }
                else
                    {
                    polcof = 1;
                    }
                break;
            case POL_RL: t120 = datum->apdata_rl[sb];
                if(station_pol_mode == LIN_MODE)
                    {
                    polcof = (pass->npols > 1) ? sin (dpar) : signum (sin (dpar));
                    }
                else if (station_pol_mode == CIRC_PAREL)
                    {
                    polcof = 1;
                    cpolrotfac = cpolvalue[pol];
                    }
                else
                    {
                    polcof = 1;
                    }
                break;
            }                       // end of switch(pol)
        polcof_sum += fabs (polcof);
                                    // sanity test
        if (t120 -> type != SPECTRAL)
            {
            msg ("Conflicting correlation type %d", 2, t120->type);
            return;
            }

        // note datum->lsbfrac or datum->usbfrac remains at -1.0
        if (pass->control.min_weight > 0.0 &&
            pass->control.min_weight > t120->fw.weight) continue;

#define STATUS_AP_ACCOUNTING 4      // 0 = original code
                                    // 1 = place it at the bottom of poln loop
                                    // 2 = place it outside the poln loop
                                    // 3 = place it after spectral edits
                                    // 4 = split things up
// version 2 updates status prior to edits
// version 3 updates status after the edits
// FIXME:
// 0 and 1 are completely equivalent and passes current suite.
// 2 is also equivalent--it just requires that the loop logic be repeated.
// 2 passes chk_passband, but the amp-scale on the time axis is wrong
//
// 3 passes chk_notches, but the amp-scale on the time axis is wrong
// 3 fails chk_passband, with incorrect int.time, low amp and amp-scale on time plots
//
// 4 places passband and notches on the same footing in that the integration time
//   and amplitudes are ok, but the SNR and amp-scale on time plots are wrong.
//
// -- oh, shit, notches test used old hardware-correlated data
// (Sep 03, we seem to be down to 4 as the correct solution,
// so the 1..3 cases are deletable, which we'll do now and
// change 'warning' to 'error'.
//
// A new status parameter is needed to pass this correction onwards.
//
// adjust the ?sbfrac but something is not quite right there anyway....
// status->total_ap for integration time, see also fill_206, fill_208
// status->total_ap_frac is an editted version and is used in make_plot_data
// and is what is used in for status.snr and status.prob_false
// the separation of the two appears to have been started in Oct200 by cjl.
// status.total_.sb_frac scale the two halves of the xp spectrum plot
//
// where do the MBD numbers come from if there is only one channel?
// these values appear to be unstable between the 4 cases

        // it's not clear why all this accounting is done here
        if (ip == lastpol[sb])      // determine data weights by sideband
            {                       // last included polarization, do totals
#if STATUS_AP_ACCOUNTING == 0
#warning "STATUS_AP_ACCOUNTING == 0"
            status->ap_num[sb][fr]++;
            status->total_ap++;
#endif /* STATUS_AP_ACCOUNTING == 0 */
                                    // sum to micro-edited totals
            if (sb)                 // lower sideband
                {                   // 0 weight encoded by negative 0
                if (*((unsigned int *)(&(t120->fw.weight))) == 0)
                                    // +0 is backward-compatibility for no weight
                                    // so: it is all present:
                    datum->lsbfrac = 1.0;
                else
                    datum->lsbfrac = t120->fw.weight;
#if STATUS_AP_ACCOUNTING == 0
                status->ap_frac[sb][fr] += datum->lsbfrac;
                status->total_ap_frac   += datum->lsbfrac;
                status->total_lsb_frac  += datum->lsbfrac;
#endif /* STATUS_AP_ACCOUNTING == 0 */
                }
            else                    // upper sideband
                {
                if (*((unsigned int *)(&(t120->fw.weight))) == 0)
                                    // so: it is all present, see above
                    datum->usbfrac = 1.0;
                else
                    datum->usbfrac = t120->fw.weight;
#if STATUS_AP_ACCOUNTING == 0
                status->ap_frac[sb][fr] += datum->usbfrac;
                status->total_ap_frac   += datum->usbfrac;
                status->total_usb_frac  += datum->usbfrac;
#endif /* STATUS_AP_ACCOUNTING == 0 */
                }
            }

                                    // add in phase effects if multitone delays 
                                    // were extracted
        if (pass->control.nsamplers && param->pc_mode[0] == MULTITONE 
                                    && param->pc_mode[1] == MULTITONE)      
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
            {                       // filter out any nan's, if present
            if (isnan (t120->ld.spec[i].re) || isnan (t120->ld.spec[i].im))
                {
                msg ("omitting nan's in visibility for ap %d fr %d lag %i", 
                      2, ap, fr, i);
                }
                                    // add in iff this is a requested pol product
            else if (param->pol & 1<<ip || param->pol == 0)
                {           
                z = t120->ld.spec[i].re + I * t120->ld.spec[i].im;
                                    // rotate each pol prod by pcal prior to adding in
                if (sb==0)
                    z = z * datum->pc_phasor[ip];
                else                // use conjugate of usb pcal tone for lsb
                    z = z * conj (datum->pc_phasor[ip]);
                                    // scale phasor by polarization coefficient
                                    // cpolrotfac is unity except for CIRC_PAREL
                z = z * polcof * cpolrotfac;

                                    // corrections to phase as fn of freq based upon 
                                    // delay calibrations

                                    // calculate offset frequency in GHz 
                                    // from DC edge for this spectral point
                deltaf = -2e-3 * i / (2e6 * param->samp_period * nlags);
                                    // but hold that thought until the cexp() below...

                // One size may not fit all.  The code below is a compromise
                // between current geodetic practice and current EHT needs.
                if (param->pc_mode[0] == MANUAL && param->pc_mode[1] == MANUAL)
                    {
                    // the correction had the wrong sign and minor O(1/nlags) error
                    // if one is trying to keep the mean phase of this channel fixed
                    phase_shift = - 1e-3 * diff_delay / (4e6 * param->samp_period);
                    phase_shift *= - (double)(nlags - 2) / (double)(nlags);
                    // per-channel phase should now be stable against delay adjustments
                    }
                else
                    {
                                    // correction to mean phase depends on sideband
                    phase_shift = - 1e-3 * diff_delay / (4e6 * param->samp_period);
                    if (sb)
                        phase_shift = -phase_shift;
                    }
                                    // apply phase ramp to spectral points 
                z = z * cexp (-2.0 * M_PI * I * (diff_delay * deltaf + phase_shift));
                xp_spec[i] += z;
                }
            }                       // bottom of lags loop
#if STATUS_AP_ACCOUNTING == 1
#error "STATUS_AP_ACCOUNTING == 1"
#endif /* STATUS_AP_ACCOUNTING == 1 */
        }                           // bottom of polarization loop

#if STATUS_AP_ACCOUNTING == 2
#error "STATUS_AP_ACCOUNTING == 2"
#endif /* STATUS_AP_ACCOUNTING == 2 */

#if STATUS_AP_ACCOUNTING == 4
#warning "STATUS_AP_ACCOUNTING == 4"
      for (ip=ips; ip<pass->pol+1; ip++)
        {
        if ((sb == 0 && usb_bypol[ip] == 0)
         || (sb == 1 && lsb_bypol[ip] == 0))
           continue;
        if (pass->control.min_weight > 0.0 &&
            pass->control.min_weight > t120->fw.weight) continue;
        if (ip == lastpol[sb])
            {
            status->ap_num[sb][fr]++;
            status->total_ap++;
            if (sb)
                {
                status->ap_frac[sb][fr] += datum->lsbfrac;
                status->total_ap_frac   += datum->lsbfrac;
                }
            else
                {
                status->ap_frac[sb][fr] += datum->usbfrac;
                status->total_ap_frac   += datum->usbfrac;
                }
            }
        }
#endif /* STATUS_AP_ACCOUNTING == 4 */

                                    // also skip over this next section, if no data
      if ((sb == 0 && usb_present == 0) || (sb == 1 && lsb_present == 0))
          continue;
                                    // yet another way of saying "no data"
      if ((sb == 0 && datum->usbfrac < 0) || (sb == 1 && datum->lsbfrac < 0))
          continue;

                                    /* apply spectral filters as needed */
      apply_cmplxbp (sb, fdata, xp_spec, nlags*2, pass);

      // direct adjustments to ?sbfrac; but also save factors in these:
      // status->sb_bw_fracs[MAXFREQ+ 0(passband) 1(notches)]sb]    fraction
      // status->sb_bw_origs[MAXFREQ+ 0(passband) 1(notches)]sb]    orig datum->?sbfrac
      // data from temp location is transferred below
      tmp_apfrac = (sb) ? datum->lsbfrac : datum->usbfrac;
      apply_passband (sb, ap, fdata, xp_spec, nlags*2, datum, status, param);
      apply_notches (sb, ap, fdata, xp_spec, nlags*2, datum, status, param);
      // NB these routines scale the spectral data upward to compensate for the
      // bits that have been zeroed.  This preserves the integral and the resulting
      // amp value, but breaks the amp scaling and SNR (fewer effective bits now).
      // Additionally, passband may delete channels which is also uncompensated.
      //
                                    // apply video bandpass correction (if so instructed)
                                    // Note: no amplitude adjustment is being made here.
      if (pass->control.vbp_correct)
          apply_video_bp (xp_spec, nlags/2, pass);

                                    // if data was filtered away...
      // ... we have a problem since total_ap_frac was already adjusted.
      // we might try to undo it, but the integration time calculation uses that
      // value in conjunction with the number of channels present (not the more detailed
      // set of sb/fr combinations actually used) to calculate the total integration time.
      // Short of refactoring all of this, we'll try to clean it up later with adjust_snr().
      if ((sb == 0 && datum->usbfrac <= 0) || (sb == 1 && datum->lsbfrac <= 0))
          {
          status->tot_sb_bw_aperr += tmp_apfrac;
          continue;
          }

#if STATUS_AP_ACCOUNTING == 3
#error "STATUS_AP_ACCOUNTING == 3"
#endif /* STATUS_AP_ACCOUNTING == 3 */

#if STATUS_AP_ACCOUNTING == 4
#warning "STATUS_AP_ACCOUNTING == 4"
      for (ip=ips; ip<pass->pol+1; ip++)
        {
        if ((sb == 0 && usb_bypol[ip] == 0)
         || (sb == 1 && lsb_bypol[ip] == 0))
           continue;
        if (pass->control.min_weight > 0.0 &&
            pass->control.min_weight > t120->fw.weight) continue;
        if (ip == lastpol[sb])
            {
            if (sb)
                {
                status->total_lsb_frac  += datum->lsbfrac;
                }
            else
                {
                status->total_usb_frac  += datum->usbfrac;
                }
            // move data from temp location to final location, priority to passband
            // NB: apply_passband() and apply_notches() clear sb_bw_* values at outset
            if (status->sb_bw_fracs[MAXFREQ+0][sb] > 0 &&
                status->sb_bw_origs[MAXFREQ+0][sb] > 0)         // passband
                {
                status->sb_bw_fracs[fr][sb] += status->sb_bw_fracs[MAXFREQ+0][sb];
                status->sb_bw_origs[fr][sb] += status->sb_bw_origs[MAXFREQ+0][sb];
                status->sb_bw_apcnt[fr][sb] += 1.0;
                }
            else if (status->sb_bw_fracs[MAXFREQ+1][sb] > 0 &&
                     status->sb_bw_origs[MAXFREQ+1][sb] > 0)   // notches
                {
                status->sb_bw_fracs[fr][sb] += status->sb_bw_fracs[MAXFREQ+1][sb];
                status->sb_bw_origs[fr][sb] += status->sb_bw_origs[MAXFREQ+1][sb];
                status->sb_bw_apcnt[fr][sb] += 1.0;
                }
            }
        }
#endif /* STATUS_AP_ACCOUNTING == 4 */

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
      if (sb == 0 && datum->usbfrac > 0.0)
          {                         // USB: accumulate xp spec, no phase offset
          for (i = ibegin; i < nlags; i++)
              {
              factor = datum->usbfrac;
              S[i] += factor * xp_spec[i];
              }
          }
      else if (sb == 1 && datum->lsbfrac > 0.0)
          {                         // LSB: accumulate conj(xp spec) with phase offset
          for (i = ibegin; i < nlags; i++)
              {
              factor = datum->lsbfrac;
                                    // DC+highest goes into middle element of the S array
              sindex = i ? 4 * nlags - i : 2 * nlags;
              S[sindex] += factor * conj (xp_spec[i] * 
                  cexp (I * (status->lsb_phoff[0] - status->lsb_phoff[1])));
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

    if( param->pol == POL_IXY)
        factor *= 2.0;              // normalize for the two pols we know we have
    else
        factor *= polcof_sum;       // will be > 1 if multiple pols included

    //Question:
    //why do we do this check? factor should never be negative (see above)
    //and if factor == 0, is this an error that should be flagged? 
    //  // and why do it here other than for the msg?
        // if (factor > 0.0)
        //     factor = 1.0 / factor;
    //Answer:
    //if neither of usbfrac or lsbfrac was set above the default (-1), then
    //no data was seen and thus the spectral array S is here set to zero.
    //That should result in zero values for datum->sbdelay, but why take chances.

    msg ("usbfrac %f lsbfrac %f polcof_sum %f factor %1f flag %x", -2, 
            datum->usbfrac, datum->lsbfrac, polcof_sum, factor, datum->flag);

                                    /* Collect the results */
    if(datum->flag != 0 && factor > 0.0)
        {
        factor = 1.0 / factor;      // turn it into a divisor
        for (i=0; i<4*nlags; i++) 
            S[i] = S[i] * factor;
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
        status->apbyfreq[fr]++;
        }
    else                            /* No data */
        {
        for (i = 0; i < nlags*2; i++) 
            datum->sbdelay[i] = 0.0;
        }
    }
