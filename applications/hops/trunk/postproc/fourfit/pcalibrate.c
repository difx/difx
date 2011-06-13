// pcalibrate extracts phase cal from the ap by ap data
// for a single frequency channel. It figures out which 
// tone(s) are to be used, forms their average phase over
// time, and corrects the resulting phase to mid-band. If
// there are multiple tones, then the phases are used
// to find the group delay within the band.
//
// Initial code, replacing code in norm() and search() rjc  2010.2.2
// multitone mode                                      rjc ~2010.10
// variable width integration for multitone mode       rjc  2011.2.22

#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "control.h"

#define FFTSIZE 256

void pcalibrate (struct type_pass *pass,
                 int fr)
    {
    int i,
        ilo,
        ihi,
        stn,
        ap,
        minind,
        indpeak,
        index[MAX_PCF],
        nin,
        ip,
        nsubs,
        ap_subint_start,
        kap,
        stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R

    double minf,
           peak,
           delay,
           fcenter,
           deltaf,
           theta,
           theta_ion;
        
    complex pc_avg[2][MAX_PCF],
            pc_adj[2],
            pc_sub[2],
            phasors[FFTSIZE],
            delay_fn[FFTSIZE],
            rotval[MAX_PCF];

    struct freq_corel *fdata;
    struct data_corel *datum,
                      *kdatum;
    struct interp_sdata *isd,
                        *ksd;

    extern struct type_param param;
    extern struct type_status status;
                                    // function prototypes
    double c_mag (complex), 
           c_phase (complex);
    complex c_zero (), 
            c_add (complex, complex), 
            c_mult (complex, complex), 
            s_mult (complex, double),
            c_mean (complex *, int),
            c_exp (double);
    
 
    fdata = pass->pass_data + fr;
                                    // loop over reference and remote stations
    for (stn = 0; stn < 2; stn++) 
        {
        if (param.pc_mode[stn] == MULTITONE)
            {                       // in multitone mode we may need to use all the tones
            ilo = 0;
            ihi = pass->npctones;
            }
        else
            {                       // in other modes, we have just a single tone to avg.
            ilo = pass->pci[stn][fr];
            ihi = ilo + 1;
            msg ("normal index %d for fr %d stn %d", 0, ilo, fr, stn);
            }
                                    // center freq (Hz), assuming Nyquist sampling
        fcenter = 0.25 / param.samp_period;

        if (fdata->pc_freqs[stn][pass->pcinband[stn][fr][ilo]] < 0.0)
            fcenter = -fcenter;     // LSB case
                                    // calculate a priori differential ionosphere rotation
                                    // FIXME - this is an ad hoc place for the ionosphere
        theta_ion = 8.42e9 * param.ionosphere[stn] / (1e6 * fdata->frequency + fcenter);
        msg ("a priori ionospheric phase for stn %d %lf rad at %g Hz", -1, 
             stn,theta_ion, 1e6 * fdata->frequency + fcenter);

        ip = 0;
        nsubs = 0;
        pc_adj[stn] = c_zero ();

                                    // loop over time within the scan
        for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++, ip++)  
            {
            if (ip % param.pc_period[stn] == 0)
                {                  // clear counters on sub-integration start
                status.pcals_accum[stn] = 0.0;
                for (i=0; i<MAX_PCF; i++)
                    pc_avg[stn][i] = c_zero();
                ap_subint_start = ap;
                }

            datum = fdata->data + ap;
                                    // create a useful ptr to this fr & ap's data
            isd = (stn == 0) ? &(datum->ref_sdata) : &(datum->rem_sdata);

                                    // add in appropriate weight
            if (stnpol[stn][param.pol])
                status.pcals_accum[stn] += isd->pcweight_rcp;
            else
                status.pcals_accum[stn] += isd->pcweight_lcp;

                                    // find time average for all tones in this channel
                                    // if in multitone mode, otherwise of single tone
            for (i=ilo; i<ihi; i++)
                pc_avg[stn][i] = (stnpol[stn][param.pol]) ?
                    c_add (isd->phasecal_rcp[pass->pcinband[stn][fr][i]], pc_avg[stn][i]):
                    c_add (isd->phasecal_lcp[pass->pcinband[stn][fr][i]], pc_avg[stn][i]);

                                    // do calculations only at sub-integration end
                                    // or for last ap, when in multitone mode
            if (ip%param.pc_period[stn] == param.pc_period[stn]-1 || ip == pass->num_ap-1)
              {
                                    // normalize the pcal phasors
              for (i=ilo; i<ihi; i++)
                  if (status.pcals_accum[stn] > 0.0)
                      pc_avg[stn][i] = s_mult (pc_avg[stn][i], 1 / status.pcals_accum[stn]);
                  else 
                      pc_avg[stn][i] = c_zero ();

              if (param.pc_mode[stn] == MULTITONE)
                {                   // in multitone mode, find peak of delay function
                                    // and correct phase to midband

                                    // debug printout of all tone contributions
                for (i=0; i<pass->npctones; i++)
                    msg ("stn %d subint ap %d..%d tone %d pc phasor %lf %lf freq %10.1lf",
                    -1, stn, ap_subint_start, ap, i, pc_avg[stn][i].re, pc_avg[stn][i].im,
                    fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]]);

                                    // find lowest (USB) or highest (LSB) tone frequency
                minf = 1e12;
                for (i=0; i<MAX_PCF; i++)
                    if (pass->pcinband[stn][fr][i] >= 0
                     && fabs (fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]]) < minf)
                        {
                        minf = fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]];
                        minind = i;
                        }
                                    // make frequencies relative to lowest/highest tone
                                    // and turn frequencies into integer indices of spacing
                for (i=0; i<MAX_PCF; i++)
                    if (pass->pcinband[stn][fr][i] >= 0)
                        {
                        index[i] = 
                         (int)(fabs (fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]] - minf)
                                 / param.pcal_spacing[stn] + 0.5);
                        }

                                    // pad with zeroes to increase sampling of transform
                for (i=0; i<FFTSIZE; i++)
                    phasors[i] = c_zero ();
                                    // fill array with complex tone values
                for (i=0; i<pass->npctones; i++)
                    if (pass->pcinband[stn][fr][i] >= 0)
                        phasors[index[i]] = pc_avg[stn][i];
                                    // fft to delay space
                FFT1 (phasors, FFTSIZE, 1, delay_fn, 1);
                                    // find peak of fft
                peak = -1.0;
                for (i=0; i<FFTSIZE; i++)
                    if (c_mag (delay_fn[i]) > peak)
                        {
                        peak = c_mag (delay_fn[i]);
                        indpeak = i;
                        }
                                    // DC is in 0th element
                indpeak = (indpeak < 128) ? indpeak : indpeak - 256;
                delay = indpeak / 256.0 / param.pcal_spacing[stn];       

                                    // interpolate to optimal value (NYI)
                                    // find corresponding delay in suitable range
                                    // find mean of delay-adjusted phases at center frequency
                nin = 0;
            
                for (i=0; i<pass->npctones; i++)
                    if (pass->pcinband[stn][fr][i] >= 0)
                        {           // rotate each value to the center frequency
                        deltaf = fcenter - fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]];
                        if (fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]] < 0.0)
                            deltaf = -deltaf;
                        theta = 2.0 * M_PI * delay * deltaf + theta_ion;

                        rotval[i] = c_mult (pc_avg[stn][i], c_exp (theta));
                        msg ("tone %d delay %g theta %lf rotated pcal phasor %lf %lf", -1, 
                             i, delay, theta, rotval[i].re, rotval[i].im);
                        nin++;
                        }
                if (nin >  0)       // use mean iff it exists
                    pc_sub[stn] = c_mean (rotval, nin);
                else
                    pc_sub[stn] = c_zero ();
                                    // write this value into all ap's of sub-int.
                for (kap=ap_subint_start; kap<=ap; kap++)
                    {
                    kdatum = fdata->data + kap;
                    ksd = (stn == 0) ? &(kdatum->ref_sdata) : &(kdatum->rem_sdata);
                    ksd->mt_pcal[stnpol[stn][param.pol]] = pc_sub[stn];
                    }
                                    // maintain a running total
                pc_adj[stn] = c_add (pc_sub[stn], pc_adj[stn]);
                nsubs++;
                }                   // end of sub-integration calculation block
              }
            }                       // end of loop over all ap's

                                    // find average pcal phasor for multitone
        if (param.pc_mode[stn] == MULTITONE)
            pc_adj[stn] = s_mult (pc_adj[stn], 1.0 / nsubs);
        else                        // if not multitone, just copy in phasor from
            {                       // single tone, and continue on with next freq
            pc_adj[stn] = pc_avg[stn][ilo];
                                    // and apply ionospheric phase
            pc_adj[stn] = c_mult (pc_adj[stn],c_exp(theta_ion));
            }

                                    // make copies of amplitude and phase
        status.pc_meas[fr][stn] = c_phase (pc_adj[stn]);
        if (param.pc_mode[stn] != MANUAL)
            status.pc_phase[fr][stn] = c_phase (pc_adj[stn]);
        else
            status.pc_phase[fr][stn] = 0.0;

        status.pc_phase[fr][stn] += M_PI * status.pc_offset[fr][stn] / 180.0;

        status.pc_amp[fr][stn] = c_mag (pc_adj[stn]);
        msg ("chan %d stn %d pc_amp %lf pc_phase %lf", 1,
             fr, stn, status.pc_amp[fr][stn], status.pc_phase[fr][stn]);
        }                           // bottom of stn = 0..1 loop
    }
