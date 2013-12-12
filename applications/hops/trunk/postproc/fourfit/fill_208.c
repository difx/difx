/************************************************************************/
/*                                                                      */
/*  Fills in a type_208 record, based on root and param information     */
/*                                                                      */
/*      Inputs:         root        scan_info struct                    */
/*                      param       lots of parameters                  */
/*                      status      even more parameters                */
/*                                                                      */
/*      Output:         t208        Filled in type_208 record           */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/* modified to also output ref station time-tagged obs.  2000.1.6  rjc  */
/************************************************************************/
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include "pass_struct.h"
#include "mk4_data.h"
#include "param_struct.h"

int
fill_208 (/* pass, param, status, t202, t208) */
struct type_pass *pass,
struct type_param *param,
struct type_status *status,
struct type_202 *t202,
struct type_208 *t208)
    {
    int fr, ap;
    extern struct mk4_sdata sdata[]; 
    struct mk4_sdata *refsd, *remsd;
    double adelay, arate, aaccel, temp, adelay_ref, arate_ref;
    double apphase_ref, ref_stn_delay, ambig;
    char qcode, errcode, tqcode[6];

    clear_208 (t208);
                                        /* Compute apriori model */
    if (compute_model (param, sdata, t202, &adelay, &arate, &aaccel,
                       &adelay_ref, &arate_ref, &ref_stn_delay) != 0)
        {
        msg ("Model computation fails in fill_208()", 2);
        return (-1);
        }
    msg ("baseline model delay, rate, accel = %g, %g, %g", 0, adelay,arate,aaccel);
                                        /* Quality/error codes */
    if (compute_qf (pass, param, status, &qcode, &errcode, tqcode) != 0)
        {
        msg ("Quality/error code computation fails in fill_208()", 2);
        return (-1);
        }
    t208->quality = qcode;
    t208->errcode = errcode;
    strncpy (t208->tape_qcode, tqcode, 6);
                                        /* Convert to usec */
    t208->adelay = adelay * 1.0e6;
    t208->arate = arate * 1.0e6;
    t208->aaccel = aaccel * 1.0e6;
                                        /* Totals, residuals, and errors */
    t208->tot_mbd = t208->adelay + status->mbd_max_global;
    t208->tot_sbd = t208->adelay + status->sbd_max;
                                        // anchor total mbd to sbd if desired
    ambig = 1.0 / status->freq_space;
    if (param->mbd_anchor == SBD)
        t208->tot_mbd += ambig * floor ((t208->tot_sbd - t208->tot_mbd) / ambig + 0.5);

    t208->tot_rate = t208->arate + status->corr_dr_max;
                                        /* ref. stn. time-tagged observables are
                                         * approximated by combining retarded a prioris
                                         * with non-retarded residuals */
    t208->tot_mbd_ref  = adelay_ref * 1e6 + status->mbd_max_global;
    t208->tot_sbd_ref  = adelay_ref * 1e6 + status->sbd_max;
                                        // anchor ref mbd as above
    if (param->mbd_anchor == SBD)
        t208->tot_mbd_ref += ambig 
                           * floor ((t208->tot_sbd_ref - t208->tot_mbd_ref) / ambig + 0.5);
    t208->tot_rate_ref = arate_ref * 1e6 + status->corr_dr_max;
    
    t208->resid_mbd = status->mbd_max_global;
    t208->resid_sbd = status->sbd_max;
    t208->resid_rate = status->corr_dr_max;
    t208->mbd_error = (float)(1.0 
                        / (2.0 * M_PI * status->freq_spread * status->snr));
                                        /* get proper weighting for sbd error estimate */
    status->sbavg = 0.0;
    for (fr = 0; fr < pass->nfreq; fr++)
        for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++) 
            status->sbavg += pass->pass_data[fr].data[ap].sband;
    status->sbavg /= status->total_ap;
    t208->sbd_error = (float)(sqrt (12.0) * status->sbd_sep * 4.0
                / (2.0 * M_PI * status->snr * (2.0 - fabs (status->sbavg) )));
    temp = status->total_ap * param->acc_period / pass->channels;
    t208->rate_error = (float)(sqrt(12.0) 
                        / ( 2.0 * M_PI * status->snr * param->ref_freq * temp));

    t208->ambiguity = 1.0 / status->freq_space;
    t208->amplitude = status->delres_max/10000.;
    t208->inc_seg_ampl = status->inc_avg_amp;
    t208->inc_chan_ampl = status->inc_avg_amp_freq;
    t208->snr = status->snr;
    t208->prob_false = status->prob_false;
    status->apphase = fmod (param->ref_freq * t208->adelay * 360.0, 360.0);
    t208->totphase = fmod (status->apphase + status->coh_avg_phase
                        * (180.0/M_PI) , 360.0);
    msg ("residual phase %f", 1, status->coh_avg_phase * (180.0/M_PI));
                                        /* Ref stn frame apriori delay usec */
    adelay_ref *= 1.0e6;
                                        /* ref_stn_delay in sec, rate in usec/sec */
    adelay_ref -= ref_stn_delay * t208->resid_rate;
    apphase_ref = fmod (param->ref_freq * adelay_ref * 360.0, 360.0);
    t208->totphase_ref = fmod (apphase_ref + status->coh_avg_phase
                        * (180.0/M_PI) , 360.0);
    t208->resphase = fmod (status->coh_avg_phase * (180.0/M_PI), 360.0);

    return (0);
    }
