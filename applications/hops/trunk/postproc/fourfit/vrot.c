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
                                        /* theta is in turns */

                                        /* fringe rate * time from central epoch */
    theta = pass->pass_data[fr].frequency * dr 
                * (param.acc_period * (ap + 0.5) + status.epoch_err[fr]);

                                        /* rotate by differential phase cal */
                                        /* Phasecal phases stored in radians */
    theta += (status.pc_phase[fr][0] - status.pc_phase[fr][1]) / (2.0 * M_PI);

                                        /* Residual mbd effect at this freq */
    theta += (pass->pass_data[fr].frequency - param.ref_freq + 0.125 * sb / status.sbd_sep) * mbd;
                                        /* effect of non-integral sbd when SSB 
                                         * correct phase to band center, based on delay */

    theta *= (-2.0 * M_PI);             /* convert to radians, and return a unit vector */
    return(c_exp(theta));
    }
