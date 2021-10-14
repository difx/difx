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
nullify_cblock (struct c_block *cb_ptr)
    {
    int i;

    cb_ptr -> cb_chain        = NULL;     /* terminate c_block chain properly */
    cb_ptr -> f_group         = WILDCARD;
    cb_ptr -> skip            = NULLINT;  
    cb_ptr -> min_weight      = NULLFLOAT;
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
    cb_ptr -> ion_smooth      = NULLINT;  
    cb_ptr -> est_pc_manual   = NULLINT;  
    cb_ptr -> vbp_correct     = NULLINT;  
    cb_ptr -> vbp_fit         = NULLINT;  

    for (i=0; i<5; i++)
        {
        cb_ptr -> vbp_coeffs[i].ref = NULLFLOAT;
        cb_ptr -> vbp_coeffs[i].rem = NULLFLOAT;
        }

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
        cb_ptr -> avxpzoom[i]            = NULLFLOAT;
        cb_ptr -> avxplopt[i]            = NULLINT;
        cb_ptr -> ion_window[i]          = NULLFLOAT;
        cb_ptr -> adhoc_file[i][0]       = 0; 
        cb_ptr -> adhoc_file_chans[i][0] = 0; 
        cb_ptr -> adhoc_flag_files[i][0] = 0; 
        cb_ptr -> plot_data_dir[i][0]    = 0;
        cb_ptr -> pc_phase_offset[i].ref = NULLFLOAT;
        cb_ptr -> pc_phase_offset[i].rem = NULLFLOAT;
        cb_ptr -> vbp_file[i][0]         = 0; 
        cb_ptr -> mount_type[i]          = NULLINT;
        }

    cb_ptr -> gen_cf_record = NULLINT;
    cb_ptr -> nnotches = NULLINT;
    for (i=0; i<MAXNOTCH; i++)
        cb_ptr -> notches[i][0] = 
        cb_ptr -> notches[i][1] = NULLFLOAT;

    for (i=0; i<4; i++)
        cb_ptr -> knot[i] = FALSE;

    for (i=0; i<32; i++)
        cb_ptr -> source[i] = WILDCARD;

    for (i=0; i<MAXFREQ; i++)
        {
        cb_ptr -> accept_sbs[i] = NULLINT;

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

        cb_ptr -> delay_offs[i].ref = NULLFLOAT;    // ##DELAY_OFFS##
        cb_ptr -> delay_offs[i].rem = NULLFLOAT;    // ##DELAY_OFFS##
        cb_ptr -> delay_offs_pol[i][0].ref = NULLFLOAT;
        cb_ptr -> delay_offs_pol[i][1].ref = NULLFLOAT;
        cb_ptr -> delay_offs_pol[i][0].rem = NULLFLOAT;
        cb_ptr -> delay_offs_pol[i][1].rem = NULLFLOAT;
        cb_ptr -> chid[i] = '\0';
        cb_ptr -> chid_rf[i] = 0.0;
        }

    for (i=0; i<2*MAXFREQ; i++)
        cb_ptr -> index[i]= NULLINT;

    return(0);
    }
