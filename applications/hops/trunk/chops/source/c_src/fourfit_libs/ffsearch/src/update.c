/*******************************************************************************
* Subroutine to update status when a new fringe is found.                      *
*  91.8.6   cmn  original code                                                 *            
*  96.11.1  rjc  fix delay rate correction to mbd                              *
*******************************************************************************/

#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

void update (struct type_pass* pass,
    int mbd_cell, double max_val, int lag, int drate_cell, int flag)
    {
    double dr_max, mbd_max, mbd_max_pre_dr_adjust;
    extern struct type_status status;
    extern struct type_param param;
    status.mb_indx = mbd_cell;
    status.sb_indx = lag;
    status.dr_indx = drate_cell;

    dr_max = drate_cell - (status.drsp_size/ 2.0);
    dr_max /= (status.drsp_size * param.ref_freq * param.acc_period);
                                                       /* convert mbd to usec */
    mbd_max = (mbd_cell - status.grid_points/2.);
    mbd_max /= (status.grid_points * status.freq_space );
    mbd_max_pre_dr_adjust = mbd_max; 
    mbd_max -= dr_max * ((pass->ap_off + 0.5) * param.acc_period 
             + status.epoch_err[0]);

                                    // keep mbd_max within search bounds
                                    // test for and handle usual case
    if (param.win_mb[1] >= param.win_mb[0])
        {
        if (mbd_max < param.win_mb[0])
            mbd_max = param.win_mb[0];
        else if (mbd_max > param.win_mb[1])
            mbd_max = param.win_mb[1];
        }
    else                            // handle wrap-around mbd window
        {
        if (mbd_max < param.win_mb[0] && mbd_max > param.win_mb[1])
            {                       // in the excluded region, adjust it
                                    // in the right direction
            if (mbd_max_pre_dr_adjust > mbd_max)
                mbd_max = param.win_mb[0];
            else
                mbd_max = param.win_mb[1];
            }
        }

    status.dr_max[lag] = dr_max;
    status.mbd_max[lag] = mbd_max;
    msg ("update, lag %d dr %g, mbd %g max_val %lf", -1, 
         lag, dr_max, mbd_max,max_val);

    if (flag == GLOBAL)
        {
        // storing results, but interp() is still to come
        // max555() uses 5x5x5 about [max_delchan(sbd index),
        // status.mbd_max_global(mbd delay), status.dr_max_global(dr)]
        status.max_delchan = lag;
        status.sbd_max = (lag - param.nlags) * status.sbd_sep;
        status.dr_max_global = dr_max;
        status.mbd_max_global = mbd_max;
        status.delres_max = max_val;
        status.search_amp = max_val;
        }
    else if (flag != LAG)
        msg ("Bad flag passed to update()", 2);
    }
