/************************************************************************/
/*                                                                      */
/* Given a baseline and the sdata structs, this routine computes the    */
/* apriori model delay, rate and acceleration at the fourfit reference  */
/* time.                                                                */
/*                                                                      */
/*      Inputs:     param               for baseline and reftime        */
/*                  sdata               type-3 file struct array        */
/*                                                                      */
/*      Output:     delay               baseline delay in secs          */
/*                  rate                baseline rate in secs/sec       */
/*                  accel               baseline accel. secs/sec/sec    */
/*                  delay_ref           delay at ref stn epoch (s)      */
/*                  rate_ref            rate  "   "   "    "   (s/s)    */
/*                  return value        0=OK, else bad                  */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/* mod. to calculate a prioris at ref stn time, for Mk3 compatibility   */
/*                                                2000.1.6   rjc        */
/************************************************************************/

/*************************
 * WARNING: On Linux 5.2, default egcs gcc compiler, optimization fails for
 * this routine.  It must be compiled with the -O0 option
*************************/

#include <math.h>
#include <stdio.h>
#include "mk4_data.h"
#include "param_struct.h"

int
compute_model (/*param, sdata, t202, delay, rate, accel, delay_ref, rate_ref,
                                                        ref_stn_delay) */
struct type_param *param,
struct mk4_sdata *sdata,
struct type_202 *t202,
double *delay,
double *rate,
double *accel,
double *delay_ref,
double *rate_ref,
double *ref_stn_delay)
    {
    int i;
    double ref_mod_start, rem_mod_start, ref_tdiff, rem_tdiff;
    double ref_int_no, ref_t, ref_t2, ref_t3, ref_t4, ref_t5;
    double rem_int_no, rem_t, rem_t2, rem_t3, rem_t4, rem_t5;
    double ref_delay, ref_rate, ref_accel;
    double rem_delay, rem_rate, rem_accel;
    double ref_doppler;
    double floor();
    struct mk4_sdata *refsd, *remsd;
    struct type_301 *ref301, *rem301;

                                        /* First, locate sdata files */
    refsd = remsd = NULL;
    for (i=0; i<MAXSTATIONS; i++)
        {
        if (sdata[i].t300 == NULL) continue;
        if (sdata[i].t300->id == param->baseline[0]) refsd = sdata + i;
        if (sdata[i].t300->id == param->baseline[1]) remsd = sdata + i;
        }
    if ((refsd == NULL) || (remsd == NULL))
        {
        msg ("Could not find stations for model in compute_model()", 2);
        return (-1);
        }
                                        /* Model start times in same units */
                                        /* as param.reftime (secs BOY)*/
                                        /* All times/intervals should be the */
                                        /* same for reference/remote, but just */
                                        /* in case this is false in future, */
                                        /* There's no harm done in computing */
                                        /* everything independently by station */
    ref_mod_start = 86400.0 *(refsd->t300->model_start.day - 1)
                   + 3600.0 * refsd->t300->model_start.hour
                   +   60.0 * refsd->t300->model_start.minute
                   +          refsd->t300->model_start.second;

    rem_mod_start = 86400.0 *(remsd->t300->model_start.day - 1)
                   + 3600.0 * remsd->t300->model_start.hour
                   + 60.0   * remsd->t300->model_start.minute
                   +          remsd->t300->model_start.second;
                                        /* Which model interval number? */
    ref_tdiff = param->reftime - ref_mod_start;
    rem_tdiff = param->reftime - rem_mod_start;
    ref_int_no = floor (ref_tdiff / (double)refsd->t300->model_interval);
    rem_int_no = floor (rem_tdiff / (double)remsd->t300->model_interval);
                                        /* Locate corresponding type 301 records */
                                        /* The delay splines should be the same */
                                        /* for all channels, so we just take */
                                        /* channel 0 */
    for (i=0; i<refsd->t300->nsplines; i++)
        {
        if (refsd->model[0].t301[i]->interval == (int)ref_int_no) break;
        }
    if (i == refsd->t300->nsplines)
        {
        msg ("Could not find correct spline coefficients for apriori model", 2);
        return (-1);
        }
    ref301 = refsd->model[0].t301[i];

    for (i=0; i<remsd->t300->nsplines; i++)
        if (remsd->model[0].t301[i]->interval == (int)rem_int_no) break;
    if (i == remsd->t300->nsplines)
        {
        msg ("Could not find correct spline coefficients for apriori model", 2);
        return (-1);
        }
    rem301 = remsd->model[0].t301[i];
                                        /* Seconds in target interval */
    ref_t = ref_tdiff - (ref_int_no * refsd->t300->model_interval);
    rem_t = rem_tdiff - (rem_int_no * remsd->t300->model_interval);
                                        /* Powers of t */
    ref_t2 = ref_t * ref_t;
    ref_t3 = ref_t2 * ref_t;
    ref_t4 = ref_t3 * ref_t;
    ref_t5 = ref_t4 * ref_t;
    rem_t2 = rem_t * rem_t;
    rem_t3 = rem_t2 * rem_t;
    rem_t4 = rem_t3 * rem_t;
    rem_t5 = rem_t4 * rem_t;
                                        /* Compute delays */
    ref_delay = ref301->delay_spline[0]
                    + ref301->delay_spline[1] * ref_t
                    + ref301->delay_spline[2] * ref_t2
                    + ref301->delay_spline[3] * ref_t3
                    + ref301->delay_spline[4] * ref_t4
                    + ref301->delay_spline[5] * ref_t5;
    rem_delay = rem301->delay_spline[0]
                    + rem301->delay_spline[1] * rem_t
                    + rem301->delay_spline[2] * rem_t2
                    + rem301->delay_spline[3] * rem_t3
                    + rem301->delay_spline[4] * rem_t4
                    + rem301->delay_spline[5] * rem_t5;
                                        /* Compute delay rates */
    ref_rate = ref301->delay_spline[1]
                    + ref301->delay_spline[2] * 2.0 * ref_t
                    + ref301->delay_spline[3] * 3.0 * ref_t2
                    + ref301->delay_spline[4] * 4.0 * ref_t3
                    + ref301->delay_spline[5] * 5.0 * ref_t4;
    rem_rate = rem301->delay_spline[1]
                    + rem301->delay_spline[2] * 2.0 * rem_t
                    + rem301->delay_spline[3] * 3.0 * rem_t2
                    + rem301->delay_spline[4] * 4.0 * rem_t3
                    + rem301->delay_spline[5] * 5.0 * rem_t4;
                                        /* Compute accelerations */
    ref_accel = 2.0 * ref301->delay_spline[2]
                    + ref301->delay_spline[3] * 6.0 * ref_t
                    + ref301->delay_spline[4] * 12.0 * ref_t2
                    + ref301->delay_spline[5] * 20.0 * ref_t3;
    rem_accel = 2.0 * rem301->delay_spline[2]
                    + rem301->delay_spline[3] * 6.0 * rem_t
                    + rem301->delay_spline[4] * 12.0 * rem_t2
                    + rem301->delay_spline[5] * 20.0 * rem_t3;
                                        /* Baseline apriori model */
    msg ("model delays, rem=%g sec, ref=%gsec", 0, rem_delay, ref_delay);
    msg ("model rates, rem=%g sec/sec, ref=%g sec/sec", 0, rem_rate, ref_rate);
    msg ("model accels, rem=%g sec/sec^2, ref=%g sec/sec^2", 0, rem_accel, ref_accel);
    *delay = rem_delay - ref_delay;
    *rate = rem_rate - ref_rate;
    *accel = rem_accel - ref_accel;

    
                                       /* now do calculations all over again at the
                                        * time of wavefront passing the ref station */
                                       /* Correct ref delay/rate for clocks */
                                       /* which are inherent in model from genaroot */
                                       /* Clock in usec, clockrate dimensionless */
    ref_delay -= t202->ref_clock * 1.0e-6;
    ref_rate -= t202->ref_clockrate;
                                       /* Adjust ref delay for approx time of ref */
                                       /* station wavefront passage, not geocenter */
                                       /* wavefront passage */
    ref_delay *= 1.0 - ref_rate;
                                       /* Doppler shift for ref stn, < 1 is redshift
                                        * do this so can correct for missing or extra
                                        * data bits in both streams (see rate calc) */
    ref_doppler = 1.0 - ref_rate;
                                       /* Which model interval number? Use adjusted */
                                       /* times */
    ref_tdiff = param->reftime - ref_mod_start - ref_delay;
    rem_tdiff = param->reftime - rem_mod_start - ref_delay;
    ref_int_no = floor (ref_tdiff / (double)refsd->t300->model_interval);
    rem_int_no = floor (rem_tdiff / (double)remsd->t300->model_interval);
                                        /* Locate corresponding type 301 records */
                                        /* The delay splines should be the same */
                                        /* for all channels, so we just take */
                                        /* channel 0 */
    for (i=0; i<refsd->t300->nsplines; i++)
        {
        if (refsd->model[0].t301[i]->interval == (int)ref_int_no) break;
        }
    if (i == refsd->t300->nsplines)
        {
        msg ("Could not find correct spline coefficients for apriori model", 2);
        return (-1);
        }
    ref301 = refsd->model[0].t301[i];

    for (i=0; i<remsd->t300->nsplines; i++)
        if (remsd->model[0].t301[i]->interval == (int)rem_int_no) break;
    if (i == remsd->t300->nsplines)
        {
        msg ("Could not find correct spline coefficients for apriori model", 2);
        return (-1);
        }
    rem301 = remsd->model[0].t301[i];
                                        /* Seconds in target interval */
    ref_t = ref_tdiff - (ref_int_no * refsd->t300->model_interval);
    rem_t = rem_tdiff - (rem_int_no * remsd->t300->model_interval);
                                        /* Powers of t */
    ref_t2 = ref_t * ref_t;
    ref_t3 = ref_t2 * ref_t;
    ref_t4 = ref_t3 * ref_t;
    ref_t5 = ref_t4 * ref_t;
    rem_t2 = rem_t * rem_t;
    rem_t3 = rem_t2 * rem_t;
    rem_t4 = rem_t3 * rem_t;
    rem_t5 = rem_t4 * rem_t;
                                        /* Compute delays */
    ref_delay = ref301->delay_spline[0]
                    + ref301->delay_spline[1] * ref_t
                    + ref301->delay_spline[2] * ref_t2
                    + ref301->delay_spline[3] * ref_t3
                    + ref301->delay_spline[4] * ref_t4
                    + ref301->delay_spline[5] * ref_t5;
    rem_delay = rem301->delay_spline[0]
                    + rem301->delay_spline[1] * rem_t
                    + rem301->delay_spline[2] * rem_t2
                    + rem301->delay_spline[3] * rem_t3
                    + rem301->delay_spline[4] * rem_t4
                    + rem301->delay_spline[5] * rem_t5;
                                        /* Compute delay rates */
    ref_rate = ref301->delay_spline[1]
                    + ref301->delay_spline[2] * 2.0 * ref_t
                    + ref301->delay_spline[3] * 3.0 * ref_t2
                    + ref301->delay_spline[4] * 4.0 * ref_t3
                    + ref301->delay_spline[5] * 5.0 * ref_t4;
    rem_rate = rem301->delay_spline[1]
                    + rem301->delay_spline[2] * 2.0 * rem_t
                    + rem301->delay_spline[3] * 3.0 * rem_t2
                    + rem301->delay_spline[4] * 4.0 * rem_t3
                    + rem301->delay_spline[5] * 5.0 * rem_t4;

                                        /* Baseline apriori model */
    msg ("refstn epoch model delays, rem=%g sec, ref=%gsec", 0, rem_delay, ref_delay);
    msg ("refstn epoch model rates, rem=%g sec/sec, ref=%g sec/sec", 0, rem_rate, ref_rate);
    *delay_ref = rem_delay - ref_delay;
    *rate_ref  = (rem_rate - ref_rate) * ref_doppler;
    *ref_stn_delay = ref_delay;

    return (0);
    }
