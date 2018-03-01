/************************************************************************/
/*                                                                      */
/* Figures out various rms values, actual and theoretical.  Puts        */
/* results in status structure                                          */
/*                                                                      */
/* Created April 17 1992 by CJL                                         */
/* Needs examination and modification for fractional AP weights         */
/*                                                                      */
/************************************************************************/
#include <math.h>
#include <stdio.h>
#include <complex.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

void
calc_rms (struct type_pass *pass)
    {
    complex vsum, vsumf, refpc, rempc, pcal, wght_phsr;
    double true_nseg, apwt, wt, totwt, totap, c;
    double wtf, wtf_dsb, wt_dsb, mean_ap, ap_in_seg, usbfrac, lsbfrac;
    double ref_tperr, rem_tperr;
    double refpcwt, rempcwt;
    double ref_scount_usb, ref_scount_lsb, rem_scount_usb, rem_scount_lsb;
    double ref_bias_usb, ref_bias_lsb, rem_bias_usb, rem_bias_lsb;
    int i, j, nfr, seg, ap, fr, trueap, apseg, nplot, nseg;
    int reflcp, remlcp;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    struct data_corel *datum;
    extern struct type_param param;
    extern struct type_status status;
    extern struct type_plot plot;
    extern int ap_per_seg;
                                        /* Get polarization state */
    reflcp = remlcp = FALSE;
    if ((param.pol == POLMASK_LL) || (param.pol == POLMASK_LR)) reflcp = TRUE;
    if ((param.pol == POLMASK_LL) || (param.pol == POLMASK_RL)) remlcp = TRUE;
                                        /* Figure out segmenting */
                                        /* Default - segment to generate 200 */
                                        /* dots across the fringe plot page */
    if (ap_per_seg == 0)
        {
        nplot = pass->nfreq + 1;
        if (nplot == 2) nplot = 1;
        nseg = 200 / nplot;
        if (nseg > pass->num_ap) nseg = pass->num_ap;
        apseg = pass->num_ap / nseg;
        }
                                        /* User override - just check for too */
                                        /* big a number */
    else
        {
        if (ap_per_seg > pass->num_ap) apseg = pass->num_ap;
        else apseg = ap_per_seg;
        }
    status.apseg = apseg;
                                        /* Number of segments, starting at AP 0 */
                                        /* and using integer apseg per segment */
    status.nseg = pass->num_ap / apseg;
                                        /* Remainder goes into last segment */
    if ((pass->num_ap % apseg) != 0) status.nseg += 1;
                                        /* Loop over all segments */
    totwt = 0.0; totap = 0.0;
    for (seg = 0; seg < status.nseg; seg++)
        {
        vsum = 0.0;
        wt = 0.0;                       /* Loop over freqs, and ap's in segment */
        wt_dsb = 0.0;                   /* Loop over freqs, and ap's in segment */
                                        /* forming vector sum */
        for(fr = 0; fr < pass->nfreq; fr++)
            {
            vsumf = 0.0;
            refpc = 0.0;
            rempc = 0.0;
            refpcwt = rempcwt = 0.0;
            wtf = 0.0;
            wtf_dsb = 0.0;
            mean_ap = 0.0;
            ap_in_seg = 0.0;
            usbfrac = lsbfrac = 0.0;
            ref_tperr = rem_tperr = 0.0;
            ref_scount_usb = ref_scount_lsb = 0.0;
            rem_scount_usb = rem_scount_lsb = 0.0;
            ref_bias_usb = ref_bias_lsb = 0.0;
            rem_bias_usb = rem_bias_lsb = 0.0;
            for (ap = seg * apseg; ap < (seg+1)*apseg; ap++)
                {
                if (ap >= pass->num_ap)
                    break;
                trueap = ap + pass->ap_off;
                datum = pass->pass_data[fr].data + trueap;
                apwt = fabs (plot.weights[fr][trueap]);
                wt += apwt;
                wtf += apwt;
                mean_ap += ap * apwt;
                                        /* Make sure we account for double */
                                        /* sideband normalization */
                if (datum->usbfrac >= 0.0) 
                    {
                    usbfrac += datum->usbfrac;
                    wtf_dsb += apwt;
                    wt_dsb += apwt;
                    }
                if (datum->lsbfrac >= 0.0) 
                    {
                    lsbfrac += datum->lsbfrac;
                    wtf_dsb += apwt;
                    wt_dsb += apwt;
                    }
                                        /* State count data */
                if (reflcp) 
                    {
                    ref_scount_usb += datum->ref_sdata.pos[0] + datum->ref_sdata.neg[0];
                    ref_scount_lsb += datum->ref_sdata.pos[1] + datum->ref_sdata.neg[1];
                    ref_bias_usb += datum->ref_sdata.bigpos[0] + datum->ref_sdata.pos[0]
                                    - datum->ref_sdata.neg[0] - datum->ref_sdata.bigneg[0];
                    ref_bias_lsb += datum->ref_sdata.bigpos[1] + datum->ref_sdata.pos[1]
                                    - datum->ref_sdata.neg[1] - datum->ref_sdata.bigneg[1];
                    }
                else 
                    {
                    ref_scount_usb += datum->ref_sdata.pos[2] + datum->ref_sdata.neg[2];
                    ref_scount_lsb += datum->ref_sdata.pos[3] + datum->ref_sdata.neg[3];
                    ref_bias_usb += datum->ref_sdata.bigpos[2] + datum->ref_sdata.pos[2]
                                    - datum->ref_sdata.neg[2] - datum->ref_sdata.bigneg[2];
                    ref_bias_lsb += datum->ref_sdata.bigpos[3] + datum->ref_sdata.pos[3]
                                    - datum->ref_sdata.neg[3] - datum->ref_sdata.bigneg[3];
                    }
                if (remlcp) 
                    {
                    rem_scount_usb += datum->rem_sdata.pos[0] + datum->rem_sdata.neg[0];
                    rem_scount_lsb += datum->rem_sdata.pos[1] + datum->rem_sdata.neg[1];
                    rem_bias_usb += datum->rem_sdata.bigpos[0] + datum->rem_sdata.pos[0]
                                    - datum->rem_sdata.neg[0] - datum->rem_sdata.bigneg[0];
                    rem_bias_lsb += datum->rem_sdata.bigpos[1] + datum->rem_sdata.pos[1]
                                    - datum->rem_sdata.neg[1] - datum->rem_sdata.bigneg[1];
                    }
                else 
                    {
                    rem_scount_usb += datum->rem_sdata.pos[2] + datum->rem_sdata.neg[2];
                    rem_scount_lsb += datum->rem_sdata.pos[3] + datum->rem_sdata.neg[3];
                    rem_bias_usb += datum->rem_sdata.bigpos[2] + datum->rem_sdata.pos[2]
                                    - datum->rem_sdata.neg[2] - datum->rem_sdata.bigneg[2];
                    rem_bias_lsb += datum->rem_sdata.bigpos[3] + datum->rem_sdata.pos[3]
                                    - datum->rem_sdata.neg[3] - datum->rem_sdata.bigneg[3];
                    }
                                        /* The actual data */
                if (apwt > 0.0)
                    totap += 1.0;
                wght_phsr = plot.phasor[fr][trueap] * apwt;
                vsum = vsum + wght_phsr;
                vsumf = vsumf + wght_phsr;
                                        /* Phasecals */
                if (param.pc_mode[0] == MULTITONE)  // reference multitone?
                    {
                    pcal = datum->ref_sdata.mt_pcal[stnpol[0][pass->pol]];
                    refpc = refpc + pcal;
                                    // add in appropriate weight
                    if (stnpol[0][pass->pol])
                        refpcwt += datum->ref_sdata.pcweight_rcp;
                    else
                        refpcwt += datum->ref_sdata.pcweight_lcp;
                    }
                else
                    {
                    pcal = datum->ref_sdata.phasecal_lcp[pass->pci[0][fr]]
                         * datum->ref_sdata.pcweight_lcp;
                    refpc = refpc + pcal;
                    pcal = datum->ref_sdata.phasecal_rcp[pass->pci[0][fr]]
                         * datum->ref_sdata.pcweight_rcp;
                    refpc = refpc + pcal;
                    refpcwt += datum->ref_sdata.pcweight_lcp + datum->ref_sdata.pcweight_rcp;
                    }

                if (param.pc_mode[1] == MULTITONE)  // remote multitone?
                    {
                    pcal = datum->rem_sdata.mt_pcal[stnpol[1][pass->pol]];
                    rempc = rempc + pcal;
                                    // add in appropriate weight
                    if (stnpol[1][pass->pol])
                        rempcwt += datum->rem_sdata.pcweight_rcp;
                    else
                        rempcwt += datum->rem_sdata.pcweight_lcp;
                    }
                else
                    {
                    pcal = datum->rem_sdata.phasecal_lcp[pass->pci[1][fr]]
                         * datum->rem_sdata.pcweight_lcp;
                    rempc = rempc + pcal;
                    pcal = datum->rem_sdata.phasecal_rcp[pass->pci[1][fr]]
                         * datum->rem_sdata.pcweight_rcp;
                    rempc = rempc + pcal;
                    rempcwt += datum->rem_sdata.pcweight_lcp + datum->rem_sdata.pcweight_rcp;
                    }
                }
                                        /* Record amp/phase in plot arrays */
                                        /* Also compute data fraction */
                                        /* tape errors, statecounts and phasecals */
            if (wtf == 0.0) 
                plot.seg_amp[fr][seg] = 0.0;
            else 
                {
                plot.mean_ap[fr][seg] = mean_ap / wtf;
                plot.seg_amp[fr][seg] = cabs (vsumf) / wtf_dsb;
                plot.seg_frac_usb[fr][seg] = usbfrac / (double)apseg;
                plot.seg_frac_lsb[fr][seg] = lsbfrac / (double)apseg;
                if (ref_scount_usb == 0.0) ref_scount_usb = -1.0; /* No data, no plot */
                if (ref_scount_lsb == 0.0) ref_scount_lsb = -1.0;
                if (rem_scount_usb == 0.0) rem_scount_usb = -1.0;
                if (rem_scount_lsb == 0.0) rem_scount_lsb = -1.0;
                plot.seg_refscnt_usb[fr][seg] = ref_scount_usb / (double)apseg;
                plot.seg_refscnt_lsb[fr][seg] = ref_scount_lsb / (double)apseg;
                plot.seg_remscnt_usb[fr][seg] = rem_scount_usb / (double)apseg;
                plot.seg_remscnt_lsb[fr][seg] = rem_scount_lsb / (double)apseg;
                if (ref_bias_usb == 0.0) ref_bias_usb = -2.0 * apseg; /* No data, no plot */
                if (ref_bias_lsb == 0.0) ref_bias_lsb = -2.0 * apseg;
                if (rem_bias_usb == 0.0) rem_bias_usb = -2.0 * apseg;
                if (rem_bias_lsb == 0.0) rem_bias_lsb = -2.0 * apseg;
                plot.seg_refbias_usb[fr][seg] = ref_bias_usb / (double)apseg;
                plot.seg_refbias_lsb[fr][seg] = ref_bias_lsb / (double)apseg;
                plot.seg_rembias_usb[fr][seg] = rem_bias_usb / (double)apseg;
                plot.seg_rembias_lsb[fr][seg] = rem_bias_lsb / (double)apseg;
                plot.seg_referr[fr][seg] = ref_tperr / (double)apseg;
                plot.seg_remerr[fr][seg] = rem_tperr / (double)apseg;
                }
            plot.seg_phs[fr][seg] = carg (vsumf);
                                        /* Pcals */
            if (param.pc_mode[0] == MANUAL)
                plot.seg_refpcal[fr][seg] = status.pc_offset[fr][0][stnpol[0][pass->pol]];
            else if (refpcwt == 0.0)
                plot.seg_refpcal[fr][seg] = 0.0;
            else 
                plot.seg_refpcal[fr][seg] = carg (refpc) * 180.0 / M_PI;

            if (param.pc_mode[1] == MANUAL)
                plot.seg_rempcal[fr][seg] = status.pc_offset[fr][1][stnpol[1][pass->pol]];
            else if (rempcwt == 0.0)
                plot.seg_rempcal[fr][seg] = 0.0;
            else 
                plot.seg_rempcal[fr][seg] = carg (rempc) * 180.0 / M_PI;
            }
                                        /* Record amp/phase for all freqs */
        if (pass->nfreq > 1)
            {
            if (wt == 0.0) plot.seg_amp[pass->nfreq][seg] = 0.0;
            else 
                {
                mean_ap = 0.0;
                nfr = 0;
                for (i=0; i<pass->nfreq; i++) 
                    {
                    mean_ap += plot.mean_ap[i][seg];
                    if (plot.mean_ap[i][seg] > 0.0) nfr++;
                    }
                if (nfr > 0)
                    plot.mean_ap[pass->nfreq][seg] = mean_ap / (double)(nfr);
                plot.seg_amp[pass->nfreq][seg] = cabs (vsum) / wt_dsb;
                }
            plot.seg_phs[pass->nfreq][seg] = carg (vsum);
            }

        c = carg(vsum) - status.coh_avg_phase;
                                        // condition to lie in [-pi,pi] interval
        c = fmod (c, 2.0 * M_PI);
        if (c > M_PI)
            c -= 2.0 * M_PI;
        else if (c < - M_PI)
            c += 2.0 * M_PI;

        status.timerms_phase += wt_dsb * c * c;
                                        /* Performs scalar sum over segments */
                                        /* of vector sums within segments and */
                                        /* over all freqs */
        c = cabs(vsum);
        status.inc_avg_amp += c * status.amp_corr_fact;
        if (wt_dsb == 0) 
            c = 0.0;
        else 
            c = c / wt_dsb;
                                        /* delres_max is amplitude at peak */
        c = c * status.amp_corr_fact - status.delres_max;
        status.timerms_amp += wt_dsb * c*c;
        totwt += wt_dsb;
        }
                                        /* This removes noise bias based on */
                                        /* SNR of each segment/freq */
    status.inc_avg_amp /= ((1.0 + (float)status.nseg/(2.0 * status.snr * status.snr)));
    status.inc_avg_amp /= totwt;


                                        /* Correct rms values for fringe segmenting */
    status.timerms_phase = sqrt(status.timerms_phase / totwt) * 180. / M_PI;
    msg ("status.delres_max = %g", 0, status.delres_max);
    status.timerms_amp = sqrt(status.timerms_amp / totwt) * 100./status.delres_max;
    msg ("status.nseg = %d",0, status.nseg);
                                        /* Calculate frequency rms values */
    for(fr=0;fr<pass->nfreq;fr++)
        {
        c = carg(status.fringe[fr]) - status.coh_avg_phase;
                                        // condition to lie in [-pi,pi] interval
        c = fmod (c, 2.0 * M_PI);
        if (c > M_PI)
            c -= 2.0 * M_PI;
        else if (c < - M_PI)
            c += 2.0 * M_PI;
        status.freqrms_phase += c * c;
        c = cabs(status.fringe[fr]) - status.delres_max;
        status.freqrms_amp += c * c;
        }
    if (pass->nfreq > 2)                // avoid 0/0 singularity
        status.freqrms_phase = sqrt(status.freqrms_phase 
                                    / (pass->nfreq - 2)) * 180./M_PI;
    else
        status.freqrms_phase = 0.0;
    status.freqrms_amp = sqrt(status.freqrms_amp / pass->nfreq) * 100. / status.delres_max;

                                        /* Theoretical RMS values */
                                        /* true_nseg is meant to be effective */
                                        /* number of segments actually included */
                                        /* in the fit for switched mode */
    true_nseg = status.nseg * totap / (pass->num_ap * pass->nfreq);

    status.th_timerms_phase = sqrt(true_nseg) * 180. / (M_PI * status.snr);
    status.th_freqrms_phase = sqrt(pass->nfreq) * 180. / (M_PI * status.snr);
        
    status.th_timerms_amp = status.th_timerms_phase * M_PI * 100. / 180.;
    status.th_freqrms_amp = status.th_freqrms_phase * M_PI * 100. / 180.;
    msg ("RMS = %lg %lg %lg %lg %lg %lg %lg %lg ",0,
        status.timerms_phase,status.timerms_amp,status.freqrms_phase,status.freqrms_amp,
        status.th_timerms_phase,status.th_timerms_amp,
        status.th_freqrms_phase,status.th_freqrms_amp);
    }
