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
// add code to copy over delay calibration delay       rjc  2011.8.9

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
        ipol,                       // 0:1 = L:R or X:Y
        lowpol,
        hipol,
        ndelpts;

    double minf,
           peak,
           delay,
           fcenter,
           deltaf,
           theta,
           theta_ion,
           delta_phase[2],
           pc_amb,
           sdelay,
           y[3],
           q[3],
           ymax,
           ampmax,
           del_avg;
        
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
    int parabola (double *, double, double, double *, double *, double *);
 
    lowpol = 0;                     // default is to do both pols
    hipol = 1;
                                    // only loop over 1 pol if that's all there is
    if (pass->npols == 1)
        {
        if (pass->pol == POL_LL)
            {
            lowpol = 0;
            hipol = 0;
            }
        else if (pass->pol == POL_RR)
            {
            lowpol = 1;
            hipol = 1;
            }
        }
        
    fdata = pass->pass_data + fr;
                                    // loop over reference and remote stations
    for (stn = 0; stn < 2; stn++) 
        {
        sdelay = (stn) ? pass->control.station_delay.rem 
                       : pass->control.station_delay.ref;
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
        theta_ion = -8.448e9 * param.ion_diff / (1e6 * fdata->frequency + fcenter);
        theta_ion = stn ?           // spread differential between ref. and remote
            0.5 * theta_ion : 
           -0.5 * theta_ion;
        msg ("a priori ionospheric phase for stn %d %lf rad at %g Hz", -1, 
             stn,theta_ion, 1e6 * fdata->frequency + fcenter);
                                    // loop over each of 2 polarizations
        for (ipol=lowpol; ipol<hipol+1; ipol++)
          {
          ip = 0;
          nsubs = 0;
          pc_adj[stn] = c_zero ();
          ndelpts = 0;
          del_avg = 0.0;
                                    // apply manual or offset pcal phase
          delta_phase[stn] = theta_ion + M_PI * status.pc_offset[fr][stn][ipol] / 180.0;
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
            if (ipol == 0)
                status.pcals_accum[stn] += isd->pcweight_lcp;
            else
                status.pcals_accum[stn] += isd->pcweight_rcp;

                                    // find time average for all tones in this channel
                                    // if in multitone mode, otherwise of single tone
            for (i=ilo; i<ihi; i++)
                pc_avg[stn][i] = (ipol == 0) ?
                    c_add (isd->phasecal_lcp[pass->pcinband[stn][fr][i]], pc_avg[stn][i]):
                    c_add (isd->phasecal_rcp[pass->pcinband[stn][fr][i]], pc_avg[stn][i]);

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
              



                                    // in multitone mode, find peak of delay function
                                    // and correct phase to midband
              if (param.pc_mode[stn] == MULTITONE)
                {
                                    // debug printout of all tone contributions
                for (i=0; i<pass->npctones; i++)
                    if (pass->pcinband[stn][fr][i] >= 0)
                        msg ("stn %d subint ap %d..%d tone %d freq %10.f pc phasor %7.2f %7.2f",
                        0, stn, ap_subint_start, ap, i,
                        fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]],
                        1e3 * c_mag(pc_avg[stn][i]), 180.0 / M_PI * c_phase(pc_avg[stn][i]));

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
                                    // interpolate to optimal value
                y[1] = peak;
                y[0] = c_mag (delay_fn[(indpeak+FFTSIZE-1)%FFTSIZE]);
                y[2] = c_mag (delay_fn[(indpeak+FFTSIZE+1)%FFTSIZE]);
                parabola (y, -1.0, 1.0, &ymax, &ampmax, q);

                                    // DC is in 0th element
                delay = (indpeak+ymax) / 256.0 / param.pcal_spacing[stn];       
                                    // find corresponding delay in suitable range
                pc_amb = 1 / param.pcal_spacing[stn];
                while (delay < sdelay - pc_amb / 2.0)
                    delay += pc_amb;
                msg ("fr %d stn %d ipol %d delay %6.1f ns", 0, fr, stn, ipol, 1e9 * delay);

                                    // find mean of delay-adjusted phases at center frequency
                nin = 0;
            
                for (i=0; i<pass->npctones; i++)
                    if (pass->pcinband[stn][fr][i] >= 0)
                        {           // rotate each value to the center frequency
                        deltaf = fcenter - fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]];
                        if (fdata->pc_freqs[stn][pass->pcinband[stn][fr][i]] < 0.0)
                            deltaf = -deltaf;
                        theta = 2.0 * M_PI * delay * deltaf;

                        rotval[i] = c_mult (pc_avg[stn][i], c_exp (theta));
                        msg ("stn %d chan %d pol %d ap %d-%d tone %d "
                             "rotated pcal phasor %7.2f %7.2f", 0, 
                             stn, fr, ipol, ap_subint_start, ap, i,
                             1e3 * c_mag(rotval[i]), 180.0 / M_PI * c_phase(rotval[i]));
                        nin++;
                        }
                if (nin >  0)       // use mean iff it exists
                    pc_sub[stn] = c_mean (rotval, nin);
                else
                    pc_sub[stn] = c_zero ();
                                    // conjugate LSB phase to USB equivalent
                if (fcenter < 0.0) 
                    pc_sub[stn].im *= -1.0;
                                    // write this value into all ap's of sub-int.
                for (kap=ap_subint_start; kap<=ap; kap++)
                    {
                    kdatum = fdata->data + kap;
                    ksd = (stn == 0) ? &(kdatum->ref_sdata) : &(kdatum->rem_sdata);
                                    // also rotate by offsets and ionosphere
                    ksd->mt_pcal[ipol] = c_mult (pc_sub[stn], c_exp(delta_phase[stn]));
                    ksd->mt_delay[ipol] = delay;
                                    // keep track of avg delay for ch/stn/pol
                    del_avg += delay;
                    ndelpts++;
                    }
                                    // maintain a running total
                pc_adj[stn] = c_add (pc_sub[stn], pc_adj[stn]);
                nsubs++;
                }                   // bottom of if multitone
              }                     // end of sub-integration calculation block
            }                       // end of loop over all ap's

                                    // find average pcal phasor for multitone
          if (param.pc_mode[stn] == MULTITONE)
              {
              pc_adj[stn] = s_mult (pc_adj[stn], 1.0 / nsubs);
              }
          else                      // if not multitone, just copy in phasor from
              {                     // single tone, and continue on with next freq
              if (param.pc_mode[stn] != MANUAL)
                  pc_adj[stn] = pc_avg[stn][ilo];
              else
                  {                 // manual pcal - set to unit amp, zero phase
                  pc_adj[stn].re = 1.0;
                  pc_adj[stn].im = 0.0;
                  }
              if (fcenter < 0.0)    // conjugate LSB phase to USB equivalent
                  pc_adj[stn].im *= -1.0;
              msg ("non-multitone pcal phasor %7.2f %7.2f", -1, 
                      1e3 * c_mag (pc_adj[stn]), 180.0 / M_PI * c_phase (pc_adj[stn]));
              }
                                    // make copies of amplitude and phase
          status.pc_meas[fr][stn][ipol] = c_phase (pc_adj[stn]);
          status.pc_phase[fr][stn][ipol] = status.pc_meas[fr][stn][ipol];
                                    // store delay average per channel, stn, and pol
          status.pc_delay[fr][stn][ipol] = del_avg / ndelpts;
                                    // rotate by iono. and offset if not already done
          if (param.pc_mode[stn] != MULTITONE)
              status.pc_phase[fr][stn][ipol] += delta_phase[stn];

          status.pc_amp[fr][stn][ipol] = c_mag (pc_adj[stn]);
          msg ("chan %d stn %d ipol %d pc_amp %6.2f pc_phase %7.2f\n", 0,
               fr, stn, ipol, 1e3 * status.pc_amp[fr][stn][ipol], 
               180.0 / M_PI * status.pc_phase[fr][stn][ipol]);

                                    // copy delay calib. values into status array
          status.delay_offs[fr][stn] = (stn) ? pass->control.delay_offs[fr].rem 
                                             : pass->control.delay_offs[fr].ref;
          }                         // end of polarizaton loop
        }                           // bottom of stn = 0..1 loop
    }
