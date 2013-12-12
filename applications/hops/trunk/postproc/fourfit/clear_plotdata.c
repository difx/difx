/************************************************************************/
/*                                                                      */
/* Make sure we have a clean slate for the plots.                       */
/*                                                                      */
/*      Inputs:         None                                            */
/*                                                                      */
/*      Output:         plot            emptied structure extern        */
/*                                                                      */
/* Created 21 December 1993 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include "mk4_data.h"
#include "plot_struct.h"
#include <stdio.h>
#include "mk4_sizes.h"

void
clear_plotdata ()
    {
    int i, j;
    complex c_zero();
    extern struct type_plot plot;

    plot.num_ap = 0;
    plot.num_freq = 0;
    plot.dr_size = 0;
    plot.dr_size_max = 0;
    for (i=0; i<8192; i++) 
        plot.mb_amp[i] = 0.0;
    for (i=0; i<MAXAP; i++) 
        {
        plot.d_rate[i] = 0.0;
        for (j=0; j<MAX_CHAN_PP+1; j++)
            {
            plot.phasor[j][i].re = 0.0;
            plot.phasor[j][i].im = 0.0;
            plot.weights[j][i] = 0.0;
            plot.seg_amp[j][i] = 0.0;
            plot.seg_phs[j][i] = 0.0;
            plot.mean_ap[j][i] = 0.0;
            plot.seg_frac_usb[j][i] = 0.0;
            plot.seg_frac_lsb[j][i] = 0.0;
            plot.seg_referr[j][i] = 0.0;
            plot.seg_remerr[j][i] = 0.0;
            plot.seg_refscnt_usb[j][i] = 0.0;
            plot.seg_refscnt_lsb[j][i] = 0.0;
            plot.seg_remscnt_usb[j][i] = 0.0;
            plot.seg_remscnt_lsb[j][i] = 0.0;
            plot.seg_refbias_usb[j][i] = 0.0;
            plot.seg_refbias_lsb[j][i] = 0.0;
            plot.seg_rembias_usb[j][i] = 0.0;
            plot.seg_rembias_lsb[j][i] = 0.0;
            plot.seg_refpcal[j][i] = 0.0;
            plot.seg_rempcal[j][i] = 0.0;
            }
        }
    for (i=0; i<MAX_CHAN_PP; i++)
        {
        plot.sb_amp[i] = 0.0;
        plot.cp_spectrum[i].re = 0.0;
        plot.cp_spectrum[i].im = 0.0;
        }
    }
