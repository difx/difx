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

    for (i=0; i<6; i++)
        cb_ptr -> adhoc_poly[i] = NULLFLOAT;

    for (i=0; i<2; i++)
	{
        cb_ptr -> baseline[i]  = WILDCARD;
        cb_ptr -> scan[i]      = NULLINT;
        cb_ptr -> sb_window[i] = NULLFLOAT;
        cb_ptr -> mb_window[i] = NULLFLOAT;
        cb_ptr -> dr_window[i] = NULLFLOAT;
        cb_ptr -> time_span[i] = NULLINT;
        cb_ptr -> passband[i]  = NULLFLOAT; 
	}

    for (i=0; i<4; i++)
	cb_ptr -> knot[i] = FALSE;

    for (i=0; i<32; i++)
	cb_ptr -> source[i] = WILDCARD;

    for (i=0; i<MAX_CHAN_PP; i++)
	{
        cb_ptr -> frequency[i] = NULLINT;

        cb_ptr -> pc_phase[i].ref = NULLFLOAT;
        cb_ptr -> pc_phase[i].rem = NULLFLOAT;

        cb_ptr -> pc_freq[i].ref = NULLFLOAT;
        cb_ptr -> pc_freq[i].rem = NULLFLOAT;

        cb_ptr -> pc_tonemask[i].ref = NULLINT;
        cb_ptr -> pc_tonemask[i].rem = NULLINT;

        cb_ptr -> gates[i].on_delay = NULLINT;
        cb_ptr -> gates[i].duration = NULLINT;
	}

    for (i=0; i<2*MAX_CHAN_PP; i++)
        cb_ptr -> index[i]= NULLINT;

    return(0);
    }
