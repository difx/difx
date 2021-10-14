/*******************************************************************************
*  Subroutine to interpolate fringes                                           *
*  and phasecal rate.                                                          *
*     8/7/91            - cmn                                                  *
*  force 0 pc_rate for manual pc                       rjc 93.6.16             *
*  Revise structure and impement windows               rjc 94.1.12             *
*  Allow search windows to "wrap around"               rjc 96.8.19             *
*  Perform incoherent averaging if desired             rjc 2006.7.19           *
*  Increase range of pcal rate peak search             rjc 2012.2.3            *
*******************************************************************************/

#include "mk4_data.h"
#include <math.h>
#include "hops_complex.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <stdio.h>

#define DR  1
#define MD 2
#define SD 3

// dmin(),dmax(), dwin() are in sub/util/minmax.c

void interp (struct type_pass *pass)
    {
    extern struct type_status status;
    extern struct type_param param;
    hops_complex X, pcal, delay[3], vrot();
    double sp, max, r_max, r, ph, peak, d_dr, d_mbd, dr, mbd,
           pcr, theta, center_mag, q[3],lower,upper,frac,
           dr_lower,dr_upper,mbd_lower,mbd_upper,sbd_lower,sbd_upper,
           dmin(),dmax(), dwin(), delay_mag[3], drpt, delta_dr, divisor, eks,
           xlim[3][2];
    int i, st, v, station, fr, lag, ap, inter, index, sbd, sband, flagbit,
        d_sbd,center_lag,ret_code, nl, ret, n, ndrpts, first;
    int isbd, imbd, idr;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    hops_complex z;
    double drf[5][5][5],
           drfmax,
           xi[3];

    struct freq_corel *frq;
    struct interp_sdata *isd;

    delay[1] = 0.0;
    status.pc_rate[0] = status.pc_rate[1] = 0.0;
    status.interp_err = 0;
                                        /* Interpolate to find phase-cal rate for normal mode */
    for (station = 0, first=TRUE; station < 2; station++)
      if (param.pc_mode[station] == NORMAL)
        {
        sp = 32.0;                      // Set initial spacing of interpolated points
                                        // ensure at most 0.5 rot of pcal phase per ap
        while (sp > 0.5 / (param.ref_freq * param.acc_period * status.rate_sep))
            sp /= 2.0;
        for (inter = 0; inter < 20; inter++)
            {                                        /* Evaluate  at 3 points */
            for (index = 0; index < 3; index++)
                {
                r = status.pc_rate[station] + status.rate_sep * sp * (index-1);
                delay[index] = 0.0;
                for (fr = 0; fr < pass->nfreq; fr++)
                    {                            /* Sum over all freqs & ap's */
                    frq = pass->pass_data + fr;
                    for (ap = pass->ap_off; ap < pass->ap_off+pass->num_ap; ap++)
                        {
                        if (station == 0)
                            isd = &(frq->data[ap].ref_sdata);
                        else if (station == 1)
                            isd = &(frq->data[ap].rem_sdata);
                                        // FIXME!! these pols shouldn't be added together
                        pcal = isd->phasecal_lcp[pass->pci[station][fr]]
                             + isd->phasecal_rcp[pass->pci[station][fr]];
                        if (cabs (pcal) > 0.0)
                            {
                            ph = carg (pcal);

                            ph -= (status.pc_phase[fr][station][stnpol[station][pass->pol]]
                               + M_PI * status.pc_offset[fr][station][stnpol[station][pass->pol]] / 180.0);

                            ph -= 2.0 * M_PI * frq->frequency * r * param.acc_period * ap;

                            msg  ("fr %d ap %d st %d freq %g pcal %f %f ph %g", -4,
                                   fr, ap, station, frq->frequency, pcal, ph);

                            delay[index] = delay[index] + cexp(I * ph);
                            }
                        }
                    }
                }
            for (i=0; i<3; i++)
                delay_mag[i] = cabs (delay[i]);
            ret = parabola (delay_mag, -1.0, 1.0, &r_max, &max, q);
            msg ("Interp: inter=%d station=%d delay_mag= [%lf,%lf,%lf] max=%lf r_max=%lf, ret=%d",-1,
                             inter,   station,   delay_mag[0],
                             delay_mag[1], delay_mag[2], max,   r_max, ret);
            msg ("st.pcr=%le st.rs=%le sp=%le",-1,
                     status.pc_rate[station], status.rate_sep, sp);
            if (ret == 0)               // only apply corrections from good fit
                status.pc_rate[station] += status.rate_sep * r_max * sp;
            else if (first)
                {
                msg ("Warning: pcal rate interpolation error for station %c",
                      2, param.baseline[station]);
                status.pc_rate[station] = 0;
                first = FALSE;          // only one message per station per baseline
                }
            status.int_pc_amp[station] = max;
            if (ret == 0)               // on successful interp, halve the interval
                sp /= 2.0;
                                        // exit loop on terminal spacing achieved
            if (sp < 0.5)
                break;
            }
        }
      else
        status.pc_rate[station] = 0.0;

    pcr = status.pc_rate[1] - status.pc_rate[0];

                                        /* Calculate Delay Res. function by
                                           rotation and summation, then
                                           interpolate to find maximum value. */

                                            /* set interpolation bounds to user
                                               windows, but constrained to lie
                                               within +/- 1 search grid point */

    dr_lower = dwin (status.dr_max_global - pcr - status.rate_sep, param.win_dr[0], param.win_dr[1]);
    dr_upper = dwin (status.dr_max_global - pcr + status.rate_sep, param.win_dr[0], param.win_dr[1]);


    mbd_lower = status.mbd_max_global - status.mbd_sep;
    mbd_upper = status.mbd_max_global + status.mbd_sep;

    if (param.win_mb[0] > param.win_mb[1])            /* is it a wrap around? */
        {
        if (status.mbd_max_global >= param.win_mb[0])
            mbd_lower = dmax (mbd_lower, param.win_mb[0]);

        else
            mbd_upper = dmin (mbd_upper, param.win_mb[1]);
        }
                              /* if wide open, don't alter the tabular points */
    else if (param.win_mb[1] - param.win_mb[0] < 0.9999 / status.freq_space)
        {                            /* otherwise make them fit within window */
        mbd_lower = dwin (mbd_lower, param.win_mb[0], param.win_mb[1]);
        mbd_upper = dwin (mbd_upper, param.win_mb[0], param.win_mb[1]);
        }

    nl = param.nlags;
    sbd_lower = dmax (param.win_sb[0], (status.max_delchan - nl - 1) * status.sbd_sep);
    sbd_upper = dmin (param.win_sb[1], (status.max_delchan - nl + 1) * status.sbd_sep);

    if (param.interpol == SIMUL)
        {
        if (status.max_delchan < 2) // condition max_delchan so 5x5x5 cube stays in data
            status.max_delchan = 2;
        else if (status.max_delchan > 2 * nl - 3)
            status.max_delchan = 2 * nl - 3;
                                    // form data cube 5x5x5 in sbd, mbd, dr
        for (isbd=0; isbd<5; isbd++)
            for (imbd=0; imbd<5; imbd++)
                for (idr=0; idr<5; idr++)
                    {
                    z = 0.0;
                                    // calculate location of this tabular point
                    sbd = status.max_delchan    +        isbd - 2;
                    mbd = status.mbd_max_global + 0.5 * (imbd - 2) * status.mbd_sep;
                    dr  = status.dr_max_global  + 0.5 * (idr - 2)  * status.rate_sep;

                    msg ("[interp]dr %le mbd %le sbd %d sbd_max(ns) %10.6f", -1,
                     dr,mbd,sbd,status.sbd_max);
                                    // counter-rotate data from all freqs. and AP's
                    for (fr = 0; fr < pass->nfreq; fr++)
                        {
                        frq = pass->pass_data + fr;
                        for (ap = pass->ap_off; ap < pass->ap_off+pass->num_ap; ap++)
                                    // only rotate if "good" flag set
                            if (frq->data[ap].flag)
                                {
                                X = frq->data[ap].sbdelay[sbd]
                                  * vrot (ap, dr, mbd, fr, 0, pass);
                                    // Weight by fractional ap's
                                frac = 0.0;
                                if (frq->data[ap].usbfrac >= 0.0)
                                    frac = frq->data[ap].usbfrac;
                                if (frq->data[ap].lsbfrac >= 0.0)
                                    frac += frq->data[ap].lsbfrac;
                                    // When both sidebands added together,
                                    // we use the mean fraction
                                if ((frq->data[ap].usbfrac >= 0.0)
                                    && (frq->data[ap].lsbfrac >= 0.0)) frac /= 2.0;
                                X = X * frac;
                                z = z + X;
                                }
                        }
                    z = z * 1.0 / status.total_ap_frac;
                    drf[isbd][imbd][idr] = cabs (z);
                    msg ("drf[%d][%d][%d] %lf", 0, isbd, imbd, idr, drf[isbd][imbd][idr]);
                    }
                                    // form the search bounds in all 3 dimensions
        xlim[0][0] = sbd_lower / status.sbd_sep - status.max_delchan + nl;
        xlim[0][1] = sbd_upper / status.sbd_sep - status.max_delchan + nl;

        xlim[1][0] = 2.0 * (mbd_lower - status.mbd_max_global) / status.mbd_sep;
        xlim[1][1] = 2.0 * (mbd_upper - status.mbd_max_global) / status.mbd_sep;

        xlim[2][0] = 2.0 * (dr_lower - status.dr_max_global) / status.rate_sep;
        xlim[2][1] = 2.0 * (dr_upper - status.dr_max_global) / status.rate_sep;
        msg ("xlim's %f %f    %f %f    %f %f", 0, xlim[0][0], xlim[0][1],
                xlim[1][0], xlim[1][1], xlim[2][0], xlim[2][1]);
                                    // find maximum value within cube via interpolation
        max555 (drf, xlim, xi, &drfmax);

                                    // calculate location of maximum in actual coords
        status.sbd_max = (status.max_delchan - nl + xi[0]) * status.sbd_sep;
        status.mbd_max_global += xi[1] * 0.5 * status.mbd_sep;
        status.dr_max_global  += xi[2] * 0.5 * status.rate_sep;
                                    // Amplitude correction due to non-zero delay rate
        theta = status.dr_max_global * param.ref_freq * param.acc_period * M_PI;
        status.amp_rate_corr = (fabs (theta) > 0.001)?
                                theta / sin (theta) :
                                1.0;
        status.amp_corr_fact = status.amp_rate_corr;
        status.delres_max = drfmax * status.amp_corr_fact;

        msg ("max555 found amp %f at sbd %f mbd %f dr %e", 1,
              status.delres_max, status.sbd_max, status.mbd_max_global, status.dr_max_global);
        // MBD is meaningless if there is only one channel
        if (status.napbyfreq == 1)
            {
            msg ("only one data channel, setting MBD(%lf) to SBD(%lf)", 1,
                status.mbd_max_global, status.sbd_max);
            status.mbd_max_global = status.sbd_max;
            }
        }
    else                            // iterative interpolation
        {
        for (inter = 1; inter < 4; inter++)
            {                                              /* Interpolate 3 times */
            sp = (inter == 1) ? 1.0 : 0.5;
            for (v = 1; v < 4; v++)             /* Interpolate once/each variable */
                {
                switch (v)            /* determine bounds for location of maximum */
                    {
                    case DR:
                        d_dr = sp;
                        d_mbd = 0.0;
                        d_sbd = 0;
                        center_lag = status.max_delchan;
                        lower = (dr_lower - status.dr_max_global)
                                / (status.rate_sep * sp);
                        upper = (dr_upper - status.dr_max_global)
                                / (status.rate_sep * sp);
                        break;

                    case MD:
                        d_dr = 0.0;
                        d_mbd = sp;
                        d_sbd = 0;
                        center_lag = status.max_delchan;
                        lower = (mbd_lower - status.mbd_max_global)
                              / (status.mbd_sep * sp);
                        upper = (mbd_upper - status.mbd_max_global)
                              / (status.mbd_sep * sp);
                        break;

                    case SD:
                        d_dr = 0.0;
                        d_mbd = 0.0;
                        d_sbd = 1;
                                    /* can't interpolate beyond edge of lag space */
                        center_lag = iwin (status.max_delchan,1,(nl-1)*2);
                        lower = sbd_lower / status.sbd_sep + nl - center_lag;
                        upper = sbd_upper / status.sbd_sep + nl - center_lag;
                        break;
                    }

                                      /* setup tabular interval for this variable */
                for (index = 0; index < 3; index++)
                    {
                    delay[index] = 0.0;

                                      /* calculate location of this tabular point */
                    dr = status.dr_max_global + status.rate_sep * d_dr * (index-1);
                    mbd = status.mbd_max_global + status.mbd_sep * d_mbd * (index-1);
                    sbd = center_lag + d_sbd * (index-1);
                    msg ("[interp]dr %le mbd %le sbd %d sbd_max(ns) %10.6f",0,
                         dr,mbd,sbd,status.sbd_max);

                                    // smooth amplitudes over delay rates, iff incoherent
                                    // averaging is requested
                    if (pass->control.t_cohere <= 0.0)
                        {           // no incoherent averaging
                                    // counter-rotate data from all freqs. and AP's
                        for (fr = 0; fr < pass->nfreq; fr++)
                            {
                            frq = pass->pass_data + fr;
                            for (ap = pass->ap_off; ap < pass->ap_off+pass->num_ap; ap++)
                                        // only rotate if "good" flag set
                                if (frq->data[ap].flag)
                                    {
                                    // temporary kludge for tests rjc 2001.1.29
                                    if (v == SD)
                                      X = frq->data[ap].sbdelay[sbd]
                                        * vrot (ap, dr, mbd, fr, 0, pass);
                                    else
                                      X = frq->data[ap].sbdelay[sbd]
                                        * vrot (ap, dr, mbd, fr, frq->data[ap].sband, pass);
                                    // Weight by fractional ap's
                                    frac = 0.0;
                                    if (frq->data[ap].usbfrac >= 0.0)
                                        frac = frq->data[ap].usbfrac;
                                    if (frq->data[ap].lsbfrac >= 0.0)
                                        frac += frq->data[ap].lsbfrac;
                                    // When both sidebands added together,
                                    // we use the mean fraction
                                    if ((frq->data[ap].usbfrac >= 0.0)
                                        && (frq->data[ap].lsbfrac >= 0.0)) frac /= 2.0;
                                    X = X * frac;
                                    delay[index] = delay[index] + X;
                                    }
                            }
                        delay[index] = delay[index] * 1.0 / status.total_ap_frac;
                        delay_mag[index] = cabs (delay[index]);
                        }
                    else            // do incoherent averaging by counter-rotating and
                        {           // summing data to form a hops_complex delay value; then, by
                                    // taking the magnitude and averaging over a range of dr
                                    // values
                        ndrpts = status.drsp_size * param.acc_period
                                                  / (2.0 * pass->control.t_cohere) + 0.5;
                        delta_dr = 1.0 / (param.acc_period * status.drsp_size);
                        msg ("interp. with incoherent avg. over %d pts, spacing %f Hz", -1,
                             2*ndrpts+1, delta_dr);
                        delay_mag[index] = 0.0;
                        divisor = 0.0;
                        for (n=-ndrpts; n<=ndrpts; n++)
                            {
                            delay[index] = 0.0;
                            drpt = dr + n * delta_dr;
                                    // counter-rotate data from all freqs. and AP's
                            for (fr = 0; fr < pass->nfreq; fr++)
                                {
                                frq = pass->pass_data + fr;
                                for (ap = pass->ap_off; ap < pass->ap_off+pass->num_ap; ap++)
                                    // only rotate if "good" flag set
                                    if (frq->data[ap].flag)
                                        {
                                    // temporary kludge for tests rjc 2001.1.29
                                        if (v == SD)
                                          X = frq->data[ap].sbdelay[sbd]
                                            * vrot (ap, drpt, mbd, fr, 0, pass);
                                        else
                                          X = frq->data[ap].sbdelay[sbd]
                                            * vrot (ap, drpt, mbd, fr, frq->data[ap].sband, pass);
                                    // Weight by fractional ap's
                                        frac = 0.0;
                                        if (frq->data[ap].usbfrac >= 0.0)
                                            frac = frq->data[ap].usbfrac;
                                        if (frq->data[ap].lsbfrac >= 0.0)
                                            frac += frq->data[ap].lsbfrac;
                                    // When both sidebands added together,
                                    // we use the mean fraction
                                        if ((frq->data[ap].usbfrac >= 0.0)
                                            && (frq->data[ap].lsbfrac >= 0.0)) frac /= 2.0;
                                        X = X * frac;
                                        delay[index] = delay[index] + X;
                                        }
                                }
                            delay[index] = delay[index] * 1.0 / status.total_ap_frac;
                            delay_mag[index] += cabs (delay[index]);
                            eks = n * pass->num_ap * param.acc_period / status.drsp_size * 2.0 * M_PI;
                            if (eks != 0.0)
                                divisor += sin(eks) / eks * sin(eks) / eks;
                            else    // avoid 0 divide on sin x / x for x = 0
                                divisor += 1.0;
                            }
                        delay_mag[index] /= divisor;
                        }
                    }
                center_mag = delay_mag[1];

                                           /* parabolic interpolation of 3 points */
                ret_code = parabola (delay_mag, lower, upper, &peak, &max, q);

                msg ("parabola r_c %d peak at %f max %f range %lf %lf input pts %f %f %f",
                      0, ret_code, peak, max, lower, upper,
                      delay_mag[0], delay_mag[1], delay_mag[2]);


                switch (v)                                     /* update location */
                    {
                    case DR:
                        status.dr_max_global += status.rate_sep * sp * peak;
                        if (inter == 1)                /* save 1st iteration amp. */
                            status.interp_amp = center_mag;

                               /* Amplitude correction due to non-zero delay rate */
                        theta = status.dr_max_global * param.ref_freq
                                * param.acc_period * M_PI;

                        status.amp_rate_corr = (fabs (theta) > 0.001)?
                            theta / sin (theta) : 1.0;

                        if (ret_code & 1)
                            status.interp_err |= WIN_EDGE_RATE;
                        if (ret_code & 2)
                            status.interp_err |= INTP_ERR_RATE;
                        break;

                    case MD:
                        if (pass->nfreq != 1)
                            {
                            status.mbd_max_global += status.mbd_sep * sp * peak;

                            if (ret_code & 1)
                                status.interp_err |= WIN_EDGE_MBD;
                            if (ret_code & 2)
                                status.interp_err |= INTP_ERR_MBD;
                            }
                        else             /* only 1 freq., ignore value and errors */
                            status.mbd_max_global = 0.0;
                        status.delres_max = max;
                        break;

                    case SD:         /* condition sbd to lie within search window */
                        status.sbd_max = (center_lag - nl + peak)
                                         * status.sbd_sep;
                        status.amp_corr_fact = max * status.amp_rate_corr / center_mag;
                        status.delres_max *= status.amp_corr_fact;
                        msg ("%f, %f, %f", -1, max, status.amp_rate_corr, center_mag);

                        if (ret_code & 1)
                            status.interp_err |= WIN_EDGE_SBD;
                        if (ret_code & 2)
                            status.interp_err |= INTP_ERR_SBD;
                    }
                }
            }
        }

                       /* looping done, correct delay rate for phase cal rate */

    status.corr_dr_max = status.dr_max_global;
    if (param.pc_mode[0] == NORMAL)
       status.corr_dr_max -= status.pc_rate[0];

    if (param.pc_mode[1] == NORMAL)
       status.corr_dr_max += status.pc_rate[1];

    msg ("phase cal rate %lg %lg", 0, status.pc_rate[0],status.pc_rate[1]);
    msg ("fringe phase = %lf", 0, carg (delay[1]) * 180.0 / M_PI);
    msg ("single band delay : %lg", 0, status.sbd_max);
    msg ("dr %lg mbd %lg dmax %lf",  0, status.corr_dr_max,
                                status.mbd_max_global, status.delres_max);
    }
