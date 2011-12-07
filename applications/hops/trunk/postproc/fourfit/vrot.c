/***************************************************
* Function to calculate complex rotator            *
* used in computing delay res. function            *
*                                                  *
*                                                  *
*                                                  *
* INPUTS:                                          *
*  ap  - accum. period index (timelike)            *
*  dr  - delay rate value                          *
*  mbd - multiband delay value                     *
*  fr  - frequency channel                         *
*  sb  - sideband  [-1, 0, 1] = [LSB, DSB, USB]    *
*  pass- pass structure (used to get freqency)     *
*                                                  *
*  8/2/91    - cmn   original C version, based on  *
*                    aeer's VROT                   *
*  2001.1.17 - rjc   fix sbd correction bug        *
*  2011.5.11 - rjc   use mbd from center of band   *
*                    that was actually observed    *
***************************************************/

#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"


complex vrot (ap, dr, mbd, fr, sb, pass)
int ap, fr, sb;
double dr, mbd;
struct type_pass *pass;
    {
    complex c_exp();
    double theta;
    extern struct type_param param;
    extern struct type_status status;
                                        // theta is in rotations

                                        /* fringe rate * time from central epoch */
    theta = pass->pass_data[fr].frequency * dr 
                * (param.acc_period * (ap + 0.5) + status.epoch_err[fr]);

                                        /* Residual mbd effect at this freq */
    if (pass->control.optimize_closure) // sacrifice mbd fit for less-noisy closure?
        {
        theta += mbd * (pass->pass_data[fr].frequency - param.ref_freq 
                        + 0.125 * sb / status.sbd_sep);
        theta += (param.nlags - status.max_delchan) * 0.125 * sb;
        }
    else
        {
        theta += mbd * (pass->pass_data[fr].frequency - param.ref_freq);
                                        /* effect of non-integral sbd iff SSB
                                         * correct phase to dc edge, based on sb delay */
        theta += (param.nlags - status.max_delchan + status.sbd_max / status.sbd_sep) * 0.125 * sb;
        }
    theta *= (-2.0 * M_PI);             // convert to radians

                                        // rotate by differential phase cal (already in radians)
    theta += status.pc_phase[fr][1] - status.pc_phase[fr][0] ;

    return (c_exp(theta));              // return unit phasor
    }
