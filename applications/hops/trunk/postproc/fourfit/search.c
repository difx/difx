/*******************************************/
/*              Search subroutine          */
/*                                         */
/*      version 1 July 2, 1991   - cmn     */
/*******************************************/


#include <stdio.h>
#include <math.h>
#include <complex.h>
#include <fftw3.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "control.h"

#define MBD_GRID_MAX 8192
int search (struct type_pass *pass)
    {
    fftw_complex *data = NULL; 
    double mb_delay[MBD_GRID_MAX]; 
    static complex rate_spectrum[MAXFREQ][MAXAP];
    static double amps[MBD_GRID_MAX][MAXAP], drtemp[MAXAP];
    int cnt, fr, i, j, station, lag, dr_index, mbd_index, ap, newmax;
    int max_mbd_cell, max_dr_cell, max_lag, drlag;
    int n, jlo, jhi;
    static int mbdmax[2*MAXLAG], drmax[2*MAXLAG];
    extern struct type_param param;
    extern struct type_status status;
    extern struct type_plot plot;
    double global_max, amp;
    static double max_amp[2*MAXLAG];
    extern int do_accounting;

    fftw_plan fftplan;

    void pcalibrate (struct type_pass *, int);

                                        // Initialization
    cnt = 0;
    status.epoch_off_cent = 0.0;
    status.total_ap = 0;
    status.total_ap_frac = 0.0;
    status.total_usb_frac = 0.0;
    status.total_lsb_frac = 0.0;
                                        // allocate fftw data array
    data = fftw_malloc (sizeof (fftw_complex) * MBD_GRID_MAX);
    if (data == NULL)
        {
        msg ("fftw_malloc() failed to allocate memory");
        return (-1);
        }
    
                                        /* Make sure data will fit (note that auto
                                         * correlations use up twice as many lags */
    if (param.nlags*param.num_ap > MAX_APXLAG)
        {
        msg ("Too many lags (%d) and/or aps (%d) for data & plot arrays (%d)",
             2, param.nlags, param.num_ap, MAX_APXLAG);
        fftw_free (data);
        return (-1);
        }
                                        /* trap for too many lags */
    if (param.nlags > MAXLAG)
        {
        msg ("Too many (%d) lags", 2, param.nlags);
        fftw_free (data);
        return (-1);
        }


    for (i = 0; i <= pass->nfreq; i++)
        status.epoch_err[i]=param.frt_offset;
   
        /* This section calls norm() which performs calculations that must         */
        /* be done on each frequency for each accumulation period. For every       */
        /* frequency, it also calculates an average phasecal & dominant sideband.  */
    
    msg ("nfreq=%d, num_ap=%d",-1,pass->nfreq, pass->num_ap);
    for (fr = 0; fr < pass->nfreq; fr++) 
        {      
        status.pcals_accum[0] = status.pcals_accum[1] = 0.0;
        
                                        // extract phase calibration for this channel
        pcalibrate (pass, fr);
        }
                                        // average delay offsets over sampler pools
    if (pass->control.nsamplers > 0)
        sampler_delays (pass);

                                        // calculate phase cal corrections
    rotate_pcal (pass);
    if (do_accounting) 
        account ("Phase cal calculations");

    for (fr = 0; fr < pass->nfreq; fr++) 
        {      
        status.ap_num[0][fr] = 0;
        status.ap_num[1][fr] = 0;
        status.ap_frac[0][fr] = 0.0;
        status.ap_frac[1][fr] = 0.0;
                                        // convert the digital results to analog equivalents
        for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++)  
            if (param.corr_type == MK4HDW)
                norm_xf (pass, fr, ap);
            else                        // use spectral version
                norm_fx (pass, fr, ap);
        
        msg ("Freq %d, ap's by sideband through norm = %d, %d", -1,
                fr, status.ap_num[0][fr], status.ap_num[1][fr]);
        }
                                        /* norm can flag data, so check we still */
                                        /* have some! */
    if (status.total_ap == 0)
        {
        msg ("Warning: No valid data for this pass for pol %d", 2, pass->pol);
        fftw_free (data);   // clean up the malloc'ed data array
        return (1);
        }
    status.epoch_off_cent = -(status.epoch_off_cent / status.total_ap + 0.5)
                * param.acc_period - param.frt_offset;
        
    freq_spacing (pass);                /* Calculate spacing of freq channels */
                                        /* trap for mbd grid having too many pts */
    if (status.grid_points > MBD_GRID_MAX)
        {
        msg ("Too many mbd grid points (%d) for array (%d), check freq sequence",
             2, status.grid_points, MBD_GRID_MAX);
        fftw_free (data);
        return (-1);
        }

    if (do_accounting) 
        account ("Normalization");

        /* This section generates the delay resolution function, and searches for   */
        /* the maximum. At each SB delay it generates a delay rate function for each */
        /* frequency. Then, for each delay rate, it calculates the delay resolution */
        /* function. It then runs through all the delay res. values to find the     */
        /* maximum.                                                                 */          
        
    search_windows (pass);  
                                        /* One of the plot arrays gets filled up */
                                        /* in the next loop, so we need to initialize */
                                        /* the arrays here, rather than later on */
                                        /* in make_plotdata() */
    memset (&plot, 0, sizeof (plot));
                                        // set up for later fft's
    fftplan = fftw_plan_dft_1d (status.grid_points, data, data, FFTW_FORWARD, FFTW_MEASURE);

    for (lag = status.win_sb[0]; lag <= status.win_sb[1]; lag++)
        {
        status.lag = lag;
                                        /* drate spectrum for each freq */
                                        /* This weighted by fractional AP */
        for (fr = 0; fr < pass->nfreq; fr++) 
            delay_rate (pass, fr, rate_spectrum[fr]);

        for (dr_index = status.win_dr[0]; dr_index <= status.win_dr[1]; dr_index++) 
            {
            status.dr = dr_index;                       /* Clear data array and */
                                                /* Fill with delay rate data */
            for (i = 0; i < status.grid_points; i++)
                data[i] = 0.0;
                
            for (fr = 0; fr < pass->nfreq; fr++)
                data[status.mb_index[fr]] = rate_spectrum[fr][dr_index];

                                                // FFT to delay resolution function
            fftw_execute (fftplan);
                                                /* Copy back, with 0 delay in center */
                                                /* Note that i might wrap around to 0 */
            i = status.win_mb[0] - 1;
            do
                {
                i = (i+1) % status.grid_points;
                j = i-status.grid_points/2;
                if (j < 0) j += status.grid_points;
                mb_delay[i] = cabs (data[j]); 
                }
            while (i != status.win_mb[1]);
                                                /* Normalize delay res. value and store */
            mbd_index = status.win_mb[0] - 1;
            do
                {
                mbd_index = (mbd_index+1) % status.grid_points;
                status.mbd = mbd_index;
                                        // mean amplitude
                amps[mbd_index][dr_index] = mb_delay[mbd_index] / status.total_ap_frac;
                }  
            while (mbd_index != status.win_mb[1]);
            }
                                        // smooth amplitudes over delay rates, iff incoherent
                                        // averaging is requested
        if (pass->control.t_cohere > 0.0)
            {
            n = status.drsp_size * param.acc_period / (2.0 * pass->control.t_cohere) + 0.5;
            msg ("convolving rate spectrum over %d resolution elements", -3, n);
        
            mbd_index = status.win_mb[0] - 1;
            do                          // smooth over each of the mbd channels
                {
                mbd_index = (mbd_index+1) % status.grid_points;
                                        // create temporary copy of this delay rate row
                for (dr_index = status.win_dr[0]; dr_index <= status.win_dr[1]; dr_index++) 
                    {
                    drtemp[dr_index] = amps[mbd_index][dr_index];
                    amps[mbd_index][dr_index] = 0.0;
                    }
                                        // average together the surrounding 2n+1 dr cells
                for (dr_index = status.win_dr[0]; dr_index <= status.win_dr[1]; dr_index++) 
                    {                   // ensure that array bounds are not exceeded
                    jlo = dr_index - n;
                    jlo = (jlo < 0) ? 0 : jlo;

                    jhi = dr_index + n;
                    jhi = (jhi < MAXAP) ? jhi : MAXAP - 1;

                    for (j=jlo; j<=jhi; j++)
                        amps[mbd_index][dr_index] += drtemp[j];
                    amps[mbd_index][dr_index] /= (jhi - jlo + 1.0);
                    }
                }  
            while (mbd_index != status.win_mb[1]);
            }
            

                                        // search over mbd and dr for maximum amplitude
        max_amp[lag] = -1.0;
        for (dr_index = status.win_dr[0]; dr_index <= status.win_dr[1]; dr_index++) 
            {
            status.dr = dr_index;

            mbd_index = status.win_mb[0] - 1;
            do
                {
                mbd_index = (mbd_index+1) % status.grid_points;
                status.mbd = mbd_index;

                if (amps[mbd_index][dr_index] > max_amp[lag])
                    {
                    max_amp[lag] = amps[mbd_index][dr_index];
                    mbdmax[lag] = mbd_index;                            
                    drmax[lag] = dr_index;
                    msg ("search: lag %d dr_index %d mbd_index %d amp %f",-3,
                              lag,   dr_index,   mbd_index,   max_amp[lag]);
                    }

                }  
            while (mbd_index != status.win_mb[1]);
            }
            
                                        /* Store mbdmax for this lag */
        update (pass, mbdmax[lag], max_amp[lag], lag, drmax[lag], LAG);
        }
                                        // no more fft's to do, delete plan
    fftw_destroy_plan (fftplan);
                                        /* search over lags for global maximum */
    global_max = -1.0;
    for (lag=status.win_sb[0]; lag <= status.win_sb[1]; lag++)
        {
        if (max_amp[lag] > global_max)
            {
            global_max = max_amp[lag];
            max_mbd_cell = mbdmax[lag];
            max_dr_cell = drmax[lag];
            max_lag = lag;
            }
        }
                                        /* Store values for overall max */
    update (pass, max_mbd_cell, global_max, max_lag, max_dr_cell, GLOBAL);

    if (global_max <= 0.0)
        {
        msg ("Probable internal data selection error, values all zero", 2);
        fftw_free (data);
        return (-1);
        }

    msg ("from search: global_max %f max_lag %d drcell %d mbdcell %d",
          0, global_max, max_lag, max_dr_cell, max_mbd_cell);
        
    msg ("finished fringe search ", 1);
    if (do_accounting) account ("Grid search");

    fftw_free (data);   // clean up the malloc'ed data array
    return (0);         /* This return should be modified to give some indication */
                        /* of whether the search was successful.                  */
    }   
