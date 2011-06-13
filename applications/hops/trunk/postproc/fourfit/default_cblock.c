/***********************************************************************
*                                                                      *
*  This routine initializes the block of memory pointed to by cb_ptr   *
*  to reasonable default values.                                       *
*                                                   RJC 92.8.28        *
*                                                                      *
***********************************************************************/

#include "control.h"
#include "mk4_data.h"
#include "mk4_sizes.h"

#define FALSE 0
#define TRUE 1

int
default_cblock (cb_ptr)
struct c_block *cb_ptr;

    {
    int i;

    cb_ptr -> skip          = FALSE;
    cb_ptr -> max_parity    = 0.001;
    cb_ptr -> pc_mode.ref   = NORMAL;
    cb_ptr -> pc_mode.rem   = NORMAL;
    cb_ptr -> pc_period.ref   = 9999;
    cb_ptr -> pc_period.rem   = 9999;
    cb_ptr -> lsb_offset.ref= 0.0;
    cb_ptr -> lsb_offset.rem= 0.0;
    cb_ptr -> x_crc         = KEEP;
    cb_ptr -> y_crc         = KEEP;
    cb_ptr -> x_slip_sync   = KEEP;
    cb_ptr -> y_slip_sync   = KEEP;
    cb_ptr -> switched_mode   = 0;                 /* default to no switching */
    cb_ptr -> switched_period = 60;
    cb_ptr -> sb_window[0]  = -1.0;
    cb_ptr -> sb_window[1]  =  1.0;
    cb_ptr -> mb_window[0]  = -1.0;
    cb_ptr -> mb_window[1]  =  1.0;
    cb_ptr -> dr_window[0]  = -1.0E-5;
    cb_ptr -> dr_window[1]  =  1.0E-5;
    cb_ptr -> ra_offset     = 0.0;
    cb_ptr -> dec_offset    = 0.0;
    cb_ptr -> time_span[0]  = 0;
    cb_ptr -> time_span[1]  = 999999999;       /* accept all data within year */
    cb_ptr -> adhoc_phase   = 0;                 /* default to no phase corr. */
    cb_ptr -> adhoc_period  = 60;
    cb_ptr -> adhoc_tref    = 0.0;
    cb_ptr -> adhoc_amp     = 0.0;
    cb_ptr -> use_samples   = FALSE;
    cb_ptr -> passband[0]   = 0.0;
    cb_ptr -> passband[1]   = 1.0E6;                     /* wide open (1 THz) */
    cb_ptr -> t_cohere      = -1.0;
    cb_ptr -> ionosphere.ref= 0.0;
    cb_ptr -> ionosphere.rem= 0.0;

    for (i=0; i<6; i++)
        cb_ptr -> adhoc_poly[i] = 0.0;       /* clear ad hoc phase polynomial */
         
    for (i=0; i<MAX_CHAN_PP; i++)
        {
        cb_ptr -> frequency[i] = DSB;        /* accept all frequency channels */
        cb_ptr -> pc_phase[i].ref = 0.0;
        cb_ptr -> pc_phase[i].rem = 0.0;
        cb_ptr -> pc_freq[i].ref = 1;        /* default to 1st phase cal tone */
        cb_ptr -> pc_freq[i].rem = 1;
        cb_ptr -> pc_tonemask[i].ref = 0;    // default is to include all tones
        cb_ptr -> pc_tonemask[i].rem = 0;
        cb_ptr -> gates[i].on_delay = 0;            /* default gate always on */
        cb_ptr -> gates[i].duration = 32767;
        }

    return(0);
    }
