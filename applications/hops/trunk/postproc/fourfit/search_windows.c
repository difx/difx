/****************************************************
*  Subroutine to set up windows                     *
*  for fringe search.                               *
*                                                   *
*   91.8.2  -  cmn  original code                   *
*   96.8.9  -  rjc  modify to allow mbd window wrap *
****************************************************/

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <stdio.h>
#include <math.h>

search_windows(pass)
struct type_pass *pass;
    {
    extern struct type_param param;
    extern struct type_status status;
    int i,
        num_mb_pts;

    double x, 
           max_dr_win, 
           max_sb_win, 
           max_mb_win, 
           mb_ambiguity,
           dwin();
   
                                                   /* Set sizes & separations */
    status.drsp_size = MAXAP;
    while ((status.drsp_size / 4) > pass->num_ap) status.drsp_size /= 2;
/*  while ((status.drsp_size / 2) > pass->num_ap) status.drsp_size /= 2; */
/*  if (status.drsp_size < 32) status.drsp_size = 32; */

    status.rate_sep = 1.0 / (status.drsp_size * param.ref_freq * param.acc_period);
    status.mbd_sep = 1.0 / (status.freq_space * status.grid_points);
    status.sbd_sep = 0.5e+6 * param.samp_period;
        
    max_dr_win = 0.5 / (param.acc_period * param.ref_freq);
    max_mb_win = 0.5 / status.freq_space;
    max_sb_win = (float)param.nlags * status.sbd_sep;
    mb_ambiguity = 2.0 * max_mb_win;
/*  printf("max_mb_win %lf\n",max_mb_win); */
  
                                           /* limit windows to natural limits */

    for (i = 0; i < 2; i++)
        {
        param.win_dr[i] = dwin (param.win_dr[i], -max_dr_win, max_dr_win);
        param.win_sb[i] = dwin (param.win_sb[i], -max_sb_win, max_sb_win);
        }

                                        /* Is the multiband window wide open? */
    if (param.win_mb[1] - param.win_mb[0] > mb_ambiguity)
        {
        param.win_mb[0] = -max_mb_win;     /* ... yes - set to natural limits */
        param.win_mb[1] =  max_mb_win;
        }
                         /* ... otherwise, restrict window edge to lie within 
                            central ambiguity region. This could result in 
                            the lower limit being larger than the upper limit, 
                            which will result in a "wrap-around" search.      */
    else if (param.win_mb[0] < -max_mb_win)
        param.win_mb[0] = fmod (param.win_mb[0], mb_ambiguity) + mb_ambiguity;

    else if (param.win_mb[1] >  max_mb_win)
        param.win_mb[1] = fmod (param.win_mb[1], mb_ambiguity);




                               /* Calculate integer indices for search params */
    for (i = 0; i < 2; i++)
        {
        status.win_sb[i] = (int)(param.win_sb[i] / status.sbd_sep + param.nlags + 0.5);
        status.win_sb[i] = iwin (status.win_sb[i], 0, param.nlags*2 - 1);
        
        status.win_dr[i] = (int)(param.win_dr[i] * status.drsp_size 
                           * param.ref_freq * param.acc_period 
                           + status.drsp_size / 2 + 0.5);
        status.win_dr[i] = iwin (status.win_dr[i], 0, status.drsp_size - 1);

        x = (param.win_dr[0] + param.win_dr[1]) * 0.5 
                        * (param.acc_period * 0.5 + param.frt_offset)
                        * status.grid_points * status.freq_space;
        status.win_mb[i] = (int)(param.win_mb[i] * status.grid_points 
                        * status.freq_space + status.grid_points / 2 + x + 0.5 );

        status.win_mb[i] = iwin (status.win_mb[i], 0, status.grid_points - 1);
        }

    num_mb_pts = status.win_mb[1] - status.win_mb[0] + 1;
    if (num_mb_pts <=0)                        /* is it a wrap-around search? */
        num_mb_pts += status.grid_points;

    status.pts_searched = num_mb_pts
                          * (status.win_sb[1] - status.win_sb[0] + 1)
                          * (status.win_dr[1] - status.win_dr[0] + 1);
    }
