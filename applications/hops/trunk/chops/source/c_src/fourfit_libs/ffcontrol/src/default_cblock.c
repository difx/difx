/***********************************************************************
*                                                                      *
*  This routine initializes the block of memory pointed to by cb_ptr   *
*  to reasonable default values.                                       *
*                                                   RJC 92.8.28        *
*                                                                      *
***********************************************************************/

#include "control.h"
#include "mk4_sizes.h"
#include <string.h>

#define FALSE 0
#define TRUE 1

int
default_cblock (struct c_block *cb_ptr)

    {
    int i;
                                                            // scalar parameters
    cb_ptr -> skip          = FALSE;
    cb_ptr -> min_weight    = 0.000;
    cb_ptr -> pc_mode.ref   = MULTITONE;
    cb_ptr -> pc_mode.rem   = MULTITONE;
    cb_ptr -> pc_period.ref   = 5;
    cb_ptr -> pc_period.rem   = 5;
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

    cb_ptr -> gen_cf_record = FALSE;
    cb_ptr -> nnotches = 0;
    for (i=0; i<MAXNOTCH; i++)
        cb_ptr -> notches[i][0] = 0.0;
        cb_ptr -> notches[i][1] = 1.0E6;

    cb_ptr -> t_cohere      = -1.0;
    cb_ptr -> ionosphere.ref= 0.0;
    cb_ptr -> ionosphere.rem= 0.0;
    cb_ptr -> dc_block      = FALSE;
    cb_ptr -> sampler_codes[0] = 0;
    cb_ptr -> nsamplers     = 0;
    cb_ptr -> optimize_closure = FALSE;
    cb_ptr -> ion_window[0]  = -20.0;
    cb_ptr -> ion_window[1]  =  20.0;
    cb_ptr -> ion_npts       = 1;
    cb_ptr -> interpolator   = SIMUL;
    cb_ptr -> station_delay.ref  = 0;   // a priori station delay (default changed from 150ns to 0 per AEN request on 02/20/18 JPB)
    cb_ptr -> station_delay.rem  = 0;
    cb_ptr -> pc_delay_l.ref= 0.0;
    cb_ptr -> pc_delay_l.rem= 0.0;
    cb_ptr -> pc_delay_r.ref= 0.0;
    cb_ptr -> pc_delay_r.rem= 0.0;
    cb_ptr -> weak_channel  = 0.5;
    cb_ptr -> pc_amp_hcode  = 0.005;
    cb_ptr -> fmatch_bw_pct = 25.0;
    cb_ptr -> mbd_anchor    = MODEL;
    cb_ptr -> ion_smooth    = FALSE;
    cb_ptr -> est_pc_manual = FALSE;
    cb_ptr -> vbp_correct   = FALSE;
    cb_ptr -> vbp_fit       = FALSE;

    for (i=0; i<2; i++)                              // clear ref and rem values
        {
        cb_ptr -> adhoc_file[i][0] = 0;
        cb_ptr -> adhoc_file_chans[i][0] = 0;
        cb_ptr -> adhoc_flag_files[i][0] = 0;
        cb_ptr -> plot_data_dir[i][0] = 0;
        cb_ptr -> pc_phase_offset[i].ref = 0;
        cb_ptr -> pc_phase_offset[i].rem = 0;
        cb_ptr -> vbp_file[i][0] = 0;
        }
                      
    for (i=0; i<5; i++)
        {                                    // clear video bandpass model coefficients
        cb_ptr -> vbp_coeffs[i].ref = 0.0;
        cb_ptr -> vbp_coeffs[i].rem = 0.0;
        }
                      
    for (i=0; i<6; i++)
        cb_ptr -> adhoc_poly[i] = 0.0;       /* clear ad hoc phase polynomial */

    for (i=0; i<MAX_SAMP; i++)               // clear sampler parameters
        {
        cb_ptr -> psamplers[i] = 0;
        cb_ptr -> sampler_delay[i][0].ref = 0.0;
        cb_ptr -> sampler_delay[i][1].ref = 0.0;
        cb_ptr -> sampler_delay[i][0].rem = 0.0;
        cb_ptr -> sampler_delay[i][1].rem = 0.0;
        }
         
    for (i=0; i<MAXFREQ; i++)
        {
        cb_ptr -> accept_sbs[i] = DSB;       /* accept all frequency channels */
        cb_ptr -> pc_phase[i][0].ref = 0.0;
        cb_ptr -> pc_phase[i][1].ref = 0.0;
        cb_ptr -> pc_phase[i][0].rem = 0.0;
        cb_ptr -> pc_phase[i][1].rem = 0.0;
        cb_ptr -> pc_freq[i].ref = 1;        /* default to 1st phase cal tone */
        cb_ptr -> pc_freq[i].rem = 1;
        cb_ptr -> pc_tonemask[i].ref = 0;    // default is to include all tones
        cb_ptr -> pc_tonemask[i].rem = 0;
        cb_ptr -> gates[i].on_delay = 0;            /* default gate always on */
        cb_ptr -> gates[i].duration = 32767;
        cb_ptr -> delay_offs[i].ref = 0.0;   // ##DELAY_OFFS##
        cb_ptr -> delay_offs[i].rem = 0.0;   // ##DELAY_OFFS##
        cb_ptr -> delay_offs_pol[i][0].ref = 0.0;
        cb_ptr -> delay_offs_pol[i][1].ref = 0.0;
        cb_ptr -> delay_offs_pol[i][0].rem = 0.0;
        cb_ptr -> delay_offs_pol[i][1].rem = 0.0;
        cb_ptr -> chid_rf[i] = 0.0;
        }

    strncpy (cb_ptr->chid, FCHARS, MAXFREQ); // default single letter freq codes abc...

    return(0);
    }
