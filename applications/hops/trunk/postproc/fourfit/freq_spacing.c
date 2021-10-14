/*  frequency channels for FFT to */
/*  multi-band delay.             */

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include <stdio.h>

#ifndef MBDMXPTS
#define MBDMXPTS 8192
#endif /* MBD_GRID_MAX == MBDMXPTS */

#ifndef MBDMULT
#define MBDMULT 4
#endif /* MBDMULT */
// 2048 == 8192 / 4 == MBDMXPTS / MBDMULT
#define GRID_PTS    (MBDMXPTS / MBDMULT)

// this routine implicitly assumes all frequencies have data
// status.mb_index[MAXFREQ] indicates where in the MBD grid a channel fr sits
// values are 0..GRID_PTS-1 but there is then zero-padding to MBDMXPTS-1

#define BOGUS_MB_INDEX (GRID_PTS * MBDMULT + 1)
// this routine could be rewritten to not use pass, param or status.

#ifndef ORIGINAL_FREQ_SPACING_CODE
// set to 1 to recover original code
#define ORIGINAL_FREQ_SPACING_CODE 0
#endif /* ORIGINAL_FREQ_SPACING_CODE */

void freq_spacing (struct type_pass *pass)
    {
    double frq,min_freq,min_space,avg_freq,spacing,index,space,fabs(),sqrt();
    int fr,i,j,div,grid_pts,spacing_ok;
    extern struct type_param param;
    extern struct type_status status;
#if ORIGINAL_FREQ_SPACING_CODE
#warning "using original freq_spacing() MBD sample space"
                                        // this assumes there is data in all channels
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
#else /* ORIGINAL_FREQ_SPACING_CODE */
#warning "using modified freq_spacing() MBD sample space"
    // this version ignores channels with no data
    int mnfr = 0, mxfr = pass->nfreq-1, frcnt = 1;
    while (status.apbyfreq[mxfr] == 0 && mxfr > mnfr) mxfr--;
    while (status.apbyfreq[mnfr] == 0 && mnfr < mxfr) mnfr++;
    min_freq = pass->pass_data[mxfr].frequency;
    min_space = pass->pass_data[mnfr].frequency;
    avg_freq = min_space;
    for (i = mnfr; i < mxfr; i++)
        {
        if (status.apbyfreq[i] == 0) continue;
        frq = pass->pass_data[i].frequency;
        if (frq < min_freq) min_freq = frq;
        avg_freq += pass->pass_data[i+1].frequency;
        frcnt ++;
        for (j = i+1; j < mxfr+1; j++)
            {
            if (status.apbyfreq[j] == 0) continue;
            space = fabs (frq - pass->pass_data[j].frequency);
            if ((space < min_space) && (space != 0))
                min_space = space;
            }
        }
    avg_freq /= frcnt;
#endif /* ORIGINAL_FREQ_SPACING_CODE */
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
            // ignore frequencies that have no data after norm_fx or norm_xf.
            if (status.apbyfreq[fr] == 0)
                {
                status.mb_index[fr] = BOGUS_MB_INDEX;
                continue;
                }
            index = (pass->pass_data[fr].frequency - min_freq) / spacing;
                                        /* Check whether all freqs */
                                        /* lie on grid points */
            if (fabs(index - (int)(index+0.5)) > 0.001)
                spacing_ok = 0;
            index = (double) (int)(index + 0.5);
            // status.mb_index[fr] = index;
                                        /* Make # of grid points the smallest  */
            for (i = 1; i < 8; i++)     /* power of 2 that will cover all points */
                if((grid_pts - 1) < index)
                    grid_pts *= 2;
            if ((index > (GRID_PTS-1)) || (index < 0))
                {
                status.space_err = 1;
                status.mb_index[fr] = BOGUS_MB_INDEX;
                }
            else
                {
                status.mb_index[fr] = index;
                }
            }
        }
        while ((div < 256) && (spacing_ok == 0));

    if (status.space_err==1)
        msg ("Frequency spacing too large for FFT, check f sequence or array dims!!", 2);
    status.freq_space = spacing;
    status.grid_points = grid_pts; 
    msg ("spacing %g grid_pts %d", 0, spacing, grid_pts);

    if(status.grid_points > GRID_PTS)
        status.grid_points = GRID_PTS;
    status.grid_points *= MBDMULT;
                                        /* This needs to account for mixed */
                                        /* single and double sideband data */
                                        /* Should it be accurately weighted? */
                                        /* Seems best diagnostic of sideband */
                                        /* presence is ap_num.  Perhaps proper */
                                        /* treatment is to invent then weight */
                                        /* ap_num_frac.  Discuss on return */
    // order of code is reversed here -- if pass->nfreq == 1 the loop is not needed...
    // freq_spread is used in fill_208 to estimate the snr of the mbd value
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
    
