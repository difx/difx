/***********************************************************************
*                                                                      *
*  This routine initializes the block of memory pointed to by cb_ptr   *
*  to initial values that can be recognized later as such. Parameter   *
*  values are null, but conditions are wild-carded to match everything *
*                                                   RJC 92.8.28        *
*                                                                      *
***********************************************************************/

#include <stdio.h>
#include "control.h"
#include "mk4_sizes.h"

#define FALSE 0
#define TRUE 1

int
nullify_cblock (cb_ptr)
struct c_block *cb_ptr;

    {
    int i;

    cb_ptr -> cb_chain        = NULL;     /* terminate c_block chain properly */
    cb_ptr -> f_group         = WILDCARD;
    cb_ptr -> skip            = NULLINT;  
    cb_ptr -> max_parity      = NULLFLOAT;
    cb_ptr -> pc_mode.ref     = NULLINT;
    cb_ptr -> pc_mode.rem     = NULLINT;
    cb_ptr -> pc_period.ref   = NULLINT;
    cb_ptr -> pc_period.rem   = NULLINT;
    cb_ptr -> lsb_offset.ref  = NULLFLOAT;
    cb_ptr -> lsb_offset.rem  = NULLFLOAT;
    cb_ptr -> x_crc           = NULLINT;
    cb_ptr -> y_crc           = NULLINT;
    cb_ptr -> x_slip_sync     = NULLINT;
    cb_ptr -> y_slip_sync     = NULLINT;
    cb_ptr -> switched_mode   = NULLINT;
    cb_ptr -> switched_period = NULLINT;
    cb_ptr -> use_samples     = NULLINT;
    cb_ptr -> ref_freq        = NULLFLOAT;
    cb_ptr -> ra_offset       = NULLFLOAT;
    cb_ptr -> dec_offset      = NULLFLOAT;
    cb_ptr -> adhoc_phase     = NULLINT;
    cb_ptr -> adhoc_tref      = NULLFLOAT;
    cb_ptr -> adhoc_period    = NULLFLOAT;
    cb_ptr -> adhoc_amp       = NULLFLOAT;
    cb_ptr -> t_cohere        = NULLFLOAT;
    cb_ptr -> ionosphere.ref  = NULLFLOAT;
    cb_ptr -> ionosphere.rem  = NULLFLOAT;
    cb_ptr -> dc_block        = NULLINT;  
    cb_ptr -> nsamplers       = NULLINT;
    cb_ptr -> sampler_codes[0]= NULLCHAR;
    cb_ptr -> optimize_closure= NULLINT;  
    cb_ptr -> ion_npts        = NULLINT;
    cb_ptr -> interpolator    = NULLINT;
    cb_ptr -> station_delay.ref = NULLFLOAT;
    cb_ptr -> station_delay.rem = NULLFLOAT;
    cb_ptr -> pc_delay_l.ref  = NULLFLOAT;
    cb_ptr -> pc_delay_l.rem  = NULLFLOAT;
    cb_ptr -> pc_delay_r.ref  = NULLFLOAT;
    cb_ptr -> pc_delay_r.rem  = NULLFLOAT;
    cb_ptr -> weak_channel    = NULLFLOAT;
    cb_ptr -> pc_amp_hcode    = NULLFLOAT;
    cb_ptr -> fmatch_bw_pct   = NULLFLOAT;
    cb_ptr -> mbd_anchor      = NULLINT;

    for (i=0; i<6; i++)
        cb_ptr -> adhoc_poly[i] = NULLFLOAT;

    for (i=0; i<MAX_SAMP; i++)
        {
        cb_ptr -> psamplers[i] = NULL;
        cb_ptr -> sampler_delay[i][0].ref = NULLFLOAT;
        cb_ptr -> sampler_delay[i][1].ref = NULLFLOAT;
        cb_ptr -> sampler_delay[i][0].rem = NULLFLOAT;
        cb_ptr -> sampler_delay[i][1].rem = NULLFLOAT;
        }

    for (i=0; i<2; i++)
        {
        cb_ptr -> baseline[i]            = WILDCARD;
        cb_ptr -> scan[i]                = NULLINT;
        cb_ptr -> sb_window[i]           = NULLFLOAT;
        cb_ptr -> mb_window[i]           = NULLFLOAT;
        cb_ptr -> dr_window[i]           = NULLFLOAT;
        cb_ptr -> time_span[i]           = NULLINT;
        cb_ptr -> passband[i]            = NULLFLOAT; 
        cb_ptr -> ion_window[i]          = NULLFLOAT;
        cb_ptr -> adhoc_file[i][0]       = 0; 
        cb_ptr -> adhoc_file_chans[i][0] = 0; 
        }

    for (i=0; i<4; i++)
        cb_ptr -> knot[i] = FALSE;

    for (i=0; i<32; i++)
        cb_ptr -> source[i] = WILDCARD;

    for (i=0; i<MAXFREQ; i++)
        {
        cb_ptr -> frequency[i] = NULLINT;

        cb_ptr -> pc_phase[i][0].ref = NULLFLOAT;
        cb_ptr -> pc_phase[i][1].ref = NULLFLOAT;
        cb_ptr -> pc_phase[i][0].rem = NULLFLOAT;
        cb_ptr -> pc_phase[i][1].rem = NULLFLOAT;

        cb_ptr -> pc_freq[i].ref = NULLFLOAT;
        cb_ptr -> pc_freq[i].rem = NULLFLOAT;

        cb_ptr -> pc_tonemask[i].ref = NULLINT;
        cb_ptr -> pc_tonemask[i].rem = NULLINT;

        cb_ptr -> gates[i].on_delay = NULLINT;
        cb_ptr -> gates[i].duration = NULLINT;

        cb_ptr -> delay_offs[i].ref = NULLFLOAT;
        cb_ptr -> delay_offs[i].rem = NULLFLOAT;
        }

    for (i=0; i<2*MAXFREQ; i++)
        cb_ptr -> index[i]= NULLINT;

    return(0);
    }
