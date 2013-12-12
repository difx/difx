/****************************************************************************
* Function to calculate complex rotator                                     *
* used in computing delay res. function                                     *
*                                                                           *
*                                                                           *
*                                                                           *
* INPUTS:                                                                   *
*  ap  - accum. period index (timelike)                                     *
*  dr  - delay rate value                                                   *
*  mbd - multiband delay value                                              *
*  fr  - frequency channel                                                  *
*  sb  - sideband  [-1, 0, 1] = [LSB, DSB, USB]                             *
*  pass- pass structure (used to get freqency)                              *
*                                                                           *
*  8/2/91    - cmn   original C version, based on aeer's VROT               *
*  2001.1.17 - rjc   fix sbd correction bug                                 *
*  2011.5.11 - rjc   use mbd from center of band that was actually observed *
*  2011.10.18- rjc   add optimize_closure to keep sb error from adding noise*
*                    to edge phase, which is used in closure, but group     *
*                    delay will be noisier, esp. with analog ch. filters    *
*  2012.1.4  - rjc   remove pcal rotation (done in norm to support          *
*                    multiple pols)                                         *
****************************************************************************/

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

                                        // Residual mbd effect at this freq
    theta += mbd * (pass->pass_data[fr].frequency - param.ref_freq);
                                        // Effect due to offset of lag where max lies
    theta += (param.nlags - status.max_delchan) * 0.125 * sb;
   
    if (pass->control.optimize_closure) // sacrifice mbd fit for less-noisy closure?
        theta += 0.125 * mbd * sb / status.sbd_sep;
       
    else                                /* effect of non-integral sbd iff SSB
                                         * correct phase to dc edge, based on sb delay */
        theta += 0.125 * status.sbd_max * sb / status.sbd_sep;
        
    theta *= (-2.0 * M_PI);             // convert to radians

    return (c_exp(theta));              // return unit phasor
    }
