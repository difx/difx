/*  frequency channels for FFT to */
/*  multi-band delay.             */

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <stdio.h>

freq_spacing (pass)
struct type_pass *pass;
    {
    double frq,min_freq,min_space,avg_freq,spacing,index,space,fabs(),sqrt();
    int fr,i,j,div,grid_pts,spacing_ok;
    extern struct type_param param;
    extern struct type_status status;
                                        /* Find the lowest frequency, the smallest */
                                        /* spacing between freqs, and the average */
                                        /* frequency. */
    min_freq = pass->pass_data[pass->nfreq-1].frequency;
    min_space = pass->pass_data[0].frequency;
    avg_freq = min_space;
    for (i = 0; i < pass->nfreq-1; i++)
        {
        frq = pass->pass_data[i].frequency;
        if (frq < min_freq) min_freq = frq;
        avg_freq += pass->pass_data[i+1].frequency;
        for (j = i+1; j < pass->nfreq; j++)
            {
            space = fabs (frq - pass->pass_data[j].frequency);
            if ((space < min_space) && (space != 0))
                min_space = space;
            }
        }
    avg_freq /= pass->nfreq;
    msg ("min_freq %lf min_space %lf avg_freq %lf", -1, min_freq, min_space, avg_freq);
    div = 1;

    do                                  /* Yup, its one of them thar do-while loops */
        {
        spacing_ok = 1;
        spacing = min_space / div;      /* Sub-divide the spacing  */        
        div++;
        grid_pts = 2;
        for (fr = 0; fr < pass->nfreq; fr++)
            {
            index = (pass->pass_data[fr].frequency - min_freq) / spacing;
                                        /* Check whether all freqs */
                                        /* lie on grid points */
            if (fabs(index - (int)(index+0.5)) > 0.001)
                spacing_ok = 0;
            index = (double) (int)(index + 0.5);
            status.mb_index[fr] = index;
                                        /* Make # of grid points the smallest  */
            for (i = 1; i < 8; i++)     /* power of 2 that will cover all points */
                if((grid_pts - 1) < index)
                    grid_pts *= 2;
            if ((index > 2047) || (index < 0))
                {
                status.space_err = 1;
                status.mb_index[fr] = 0;
                }
            }
        }
        while ((div < 256) && (spacing_ok == 0));

    if (status.space_err==1)
        msg ("Frequency spacing too large for FFT, check f sequence or array dims!!", 2);
    status.freq_space = spacing;
    status.grid_points = grid_pts; 
    msg ("spacing %g grid_pts %d", 0, spacing, grid_pts);

    if(status.grid_points > 2048)
        status.grid_points = 2048;
    status.grid_points *= 4;
                                        /* This needs to account for mixed */
                                        /* single and double sideband data */
                                        /* Should it be accurately weighted? */
                                        /* Seems best diagnostic of sideband */
                                        /* presence is ap_num.  Perhaps proper */
                                        /* treatment is to invent then weight */
                                        /* ap_num_frac.  Discuss on return */
    status.freq_spread = 0.0;
    for (fr = 0; fr < pass->nfreq; fr++)
        {
        status.freq_spread += (pass->pass_data[fr].frequency - avg_freq) 
                                * (pass->pass_data[fr].frequency - avg_freq);
        msg ("freq[%d] %f mb_index %d", 0, fr, pass->pass_data[fr].frequency, status.mb_index[fr]);
        }
    if (pass->nfreq > 1) 
        status.freq_spread= sqrt(status.freq_spread / pass->nfreq);
    else if (pass->nfreq == 1) 
        status.freq_spread = 1.0 / (param.samp_period * 2.0E6 * sqrt(12.0));
    msg ("grid_points %d freq_spread %lf", 0, status.grid_points, status.freq_spread);
    }
    
