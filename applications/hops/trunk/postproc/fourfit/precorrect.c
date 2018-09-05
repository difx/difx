/*********************************************************************
*                                                                    *
*  This routine does two things:                                     *
*     1) loads parameters necessary for the fringe search into param *
*     2) makes any necessary precorrections to the data, or the      *
*        derived information (such as phase cal)                     *
*                                                                    *
*  92.8.26   rjc First written                                       *
*  94.11.21  rjc Put ph. cal phases into offset array for all 3 modes*
*  2009.8.27 rjc Allow pcal freqs to be entered as tone #'s          *  
*  2010.2.3  rjc consolidate remote and reference code into 1 loop   *
*********************************************************************/
#include "mk4_data.h"
#include "ovex.h"
#include "pass_struct.h"
#include "param_struct.h"
#include "control.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <complex.h>

extern void   account (char *);
extern void   msg (char *, int, ...);
extern int fcode(char c);

int precorrect (struct scan_struct* ovex, struct type_pass* pass)
    {
    int i, j, k, stn, n, fr, ntones, nin, mask, chind;
    double delay_offset, rate_offset, pcfreq_hz,
           pcphas[2][2],            // indexed by [stn][pol]
           pcfreq[2],
           static_pc_off;
    extern struct type_status status;
    extern struct type_param param;
    extern int do_accounting;
    static double conrad = 0.01745329252;

                                    /* copy over ref. frequency */
    param.ref_freq = (pass->control.ref_freq == NULLFLOAT)?
                     pass->pass_data[0].frequency :
                     pass->control.ref_freq;
                                         
    param.cor_limit = 16000.0;     /* Initialize large number threshold */
    param.use_sample_cnts = pass->control.use_samples;
                                    // get interpolation method for this pass
    param.interpol = pass->control.interpolator;
                                    // calculate offsets of windows due to position offsets
                            
    // delay_offset = (pass->control.ra_offset  * rbase->t2600.u_obsfreq
    //               + pass->control.dec_offset * rbase->t2600.v_obsfreq) * 1e-3;
    //
    // rate_offset =  (pass->control.ra_offset  * rbase->t2600.u_obsfreq_rate
    //               + pass->control.dec_offset * rbase->t2600.v_obsfreq_rate) * 1e-6;

    delay_offset = 0.0;             // for now, just force 0 offset  rjc 99.8.13
    rate_offset = 0.0;
    
    param.ion_pts = pass->control.ion_npts;
    param.mbd_anchor = pass->control.mbd_anchor;
    param.est_pc_manual = pass->control.est_pc_manual;

    for (i=0; i<2; i++)             // Copy windows into working area
        {
        param.win_sb[i] = pass->control.sb_window[i] + delay_offset;
        param.win_mb[i] = pass->control.mb_window[i] + delay_offset;
        param.win_dr[i] = pass->control.dr_window[i] + rate_offset;
        param.passband[i] = pass->control.passband[i];
                                    // ionosphere window is about differential a priori
                                    // collapses if only 1 pt (i.e. no search)
        param.win_ion[i] = (param.ion_pts > 1) ?
            pass->control.ion_window[i] : 0.0;
        param.win_ion[i] += (pass->control.ionosphere.rem == NULLFLOAT) ? 
                           0.0 : pass->control.ionosphere.rem;
        param.win_ion[i] -= (pass->control.ionosphere.ref == NULLFLOAT) ? 
                           0.0 : pass->control.ionosphere.ref;
        }
    param.nnotches = pass->control.nnotches;
    for (j=0; j<pass->control.nnotches; j++)
        {
        param.notches[j][0] = pass->control.notches[j][0];
        param.notches[j][1] = pass->control.notches[j][1];
        }
    param.gen_cf_record = pass->control.gen_cf_record;
    if (param.gen_cf_record) msg("CF Record will be generated",1);

    param.pc_mode[0] = pass->control.pc_mode.ref;
    param.pc_mode[1] = pass->control.pc_mode.rem;
    param.pc_period[0] = pass->control.pc_period.ref;
    param.pc_period[1] = pass->control.pc_period.rem;
   
    status.lsb_phoff[0] = pass->control.lsb_offset.ref * conrad;
    status.lsb_phoff[1] = pass->control.lsb_offset.rem * conrad;

    param.dc_block = pass->control.dc_block;
    param.weak_channel = pass->control.weak_channel;
    param.pc_amp_hcode = pass->control.pc_amp_hcode;
    param.fmatch_bw_pct = pass->control.fmatch_bw_pct;
    param.ion_smooth = pass->control.ion_smooth;
                                    // Copy phase cal offsets; identify desired pcal tone freqs
    for (stn=0; stn<2; stn++)
        {
        n = param.ov_bline[stn];
        param.pcal_spacing[stn] = ovex->st[n].channels[0].pcal_spacing;
        
        for (fr = 0; fr < pass->nfreq; fr++)  
            {
            j = fcode(pass->pass_data[fr].freq_code);
            chind = pass->pass_data[fr].ch_idx[stn];

                                    // copy delay calib. values into status array
            status.delay_offs[fr][stn] = (stn) ? pass->control.delay_offs[j].rem 
                                               : pass->control.delay_offs[j].ref;

            if (param.pc_mode[stn] != MULTITONE)
                {                   // single tone used in this frequency band, process it
                                    // find corresponding freq index in control structure
                if (stn == 0)
                    {
                    for (i=0; i<2; i++)
                        pcphas[stn][i] = pass->control.pc_phase[j][i].ref;
                    pcfreq[stn] = pass->control.pc_freq[j].ref;
                    }
                else
                    {
                    for (i=0; i<2; i++)
                        pcphas[stn][i] = pass->control.pc_phase[j][i].rem;
                    pcfreq[stn] = pass->control.pc_freq[j].rem;
                    }
                   
                for (i=0; i<2; i++)
                    {
                    status.pc_offset[fr][stn][i] = (pcphas[stn][i] != NULLFLOAT) ? pcphas[stn][i] : 0.0;
                                    //add the static (constant across all channels) phase offset for each pol
                    static_pc_off = (stn == 0)
                                  ? pass->control.pc_phase_offset[i].ref
                                  : pass->control.pc_phase_offset[i].rem;
                    status.pc_offset[fr][stn][i] += (static_pc_off != NULLFLOAT) ? static_pc_off : 0.0;
                    }

                                    // expand tone #'s into frequencies, if necessary
                if (fabs (pcfreq[stn]) > 64)
                    pcfreq_hz = 1e3 * pcfreq[stn];
                    
                else                // specified as tone #, rather than freq
                    {               // must compute frequency for this tone
                                    // assume for now that all ovex channels the same spacing
                    pcfreq_hz = fmod (pass->pass_data[fr].frequency * 1e6
                                    - ovex->st[n].channels[chind].pcal_base_freq,
                                      ovex->st[n].channels[chind].pcal_spacing);

                                    // pcfreq_hz is positive distance from next lower line
                    if (pcfreq_hz < 0.0)
                        pcfreq_hz += ovex->st[n].channels[chind].pcal_spacing;

                                    // nearest freq rail depends on sideband
                    if (ovex->st[n].channels[chind].net_sideband == 'U')
                        {               // distance to next line is complement
                        pcfreq_hz = ovex->st[n].channels[chind].pcal_spacing - pcfreq_hz;
                                    // now set freq to tone # within band (1 relative)
                        pcfreq_hz += (pcfreq[stn] - 1) * ovex->st[n].channels[chind].pcal_spacing;
                        }
                    else            // set up tone frequency for net LSB
                        {           // lsb implemented using negative frequencies
                                    // now set freq to tone # within band (1 relative)
                        pcfreq_hz = - pcfreq_hz 
                                    + (pcfreq[stn] + 1) * ovex->st[n].channels[chind].pcal_spacing;
                        }
                    msg ("fcode %c freq %lf tone request %lf pc_freqhz %lf", 0,
                          pass->pass_data[fr].freq_code, pass->pass_data[fr].frequency, 
                          pcfreq[stn], pcfreq_hz);
                    }

                for (k=0; k<MAX_PCF; k++)
                    if (fabs (pcfreq_hz - pass->pass_data[fr].pc_freqs[stn][k]) < 1e-6)
                        break;
                    else
                        msg ("pcal freq %lf didn't match", -2, 
                             pass->pass_data[fr].pc_freqs[stn][k]);
                                    // if requested frequency not available,
                                    // complain about it and use the 1st tone
                if (k == MAX_PCF)
                    {
                    k = 0;
                    pcfreq_hz = pass->pass_data[fr].pc_freqs[stn][k];
                    if (param.pc_mode[stn] != MANUAL)
                        msg ("stn %d pcal tone of %g Hz unavailable for channel %c",
                         1, stn, pcfreq_hz, pass->pass_data[fr].freq_code);
                    }
                pass->pci[stn][fr] = k;
                pass->pcinband[stn][fr][k] = k;
                msg ("stn %d using pcal tone #%d of %lf Hz for freq %d code %c",
                     0, stn, k, pcfreq_hz, fr, pass->pass_data[fr].freq_code);
                }                   // end of non-multitone mode case
            else
                {                   // process all tones in multitone mode
                for (i=0; i<2; i++)
                    pcphas[stn][i] = (stn == 0) ? pass->control.pc_phase[j][i].ref
                                                : pass->control.pc_phase[j][i].rem;
                                    // apply a priori phase offset to each pol
                for (i=0; i<2; i++)
                {
                    status.pc_offset[fr][stn][i] = (pcphas[stn][i] != NULLFLOAT) ? pcphas[stn][i] 
                                                                                 : 0.0;
                                    //add the static (constant across all channels) phase offset for each pol
                    static_pc_off = (stn == 0)
                                  ? pass->control.pc_phase_offset[i].ref 
                                  : pass->control.pc_phase_offset[i].rem;
                                                      
                    status.pc_offset[fr][stn][i] += (static_pc_off != NULLFLOAT) ? static_pc_off 
                                                                                 : 0.0;
                }

                                    // assume for now that all ovex channels the same spacing
                pcfreq_hz = fmod (pass->pass_data[fr].frequency * 1e6
                                - ovex->st[n].channels[chind].pcal_base_freq,
                                  ovex->st[n].channels[chind].pcal_spacing);

                                    // pcfreq_hz is positive distance from next lower line
                if (pcfreq_hz <= 0.0)
                    pcfreq_hz += ovex->st[n].channels[chind].pcal_spacing;
                                    // nearest freq rail depends on sideband
                if (ovex->st[n].channels[chind].net_sideband == 'U')
                                    // for USB, distance to next line is complement
                    pcfreq_hz = ovex->st[n].channels[chind].pcal_spacing - pcfreq_hz;
                                    // set up tone frequency for net LSB
                else                // lsb implemented using negative frequencies
                    pcfreq_hz = - pcfreq_hz;
                                    // calculate the number of tones in the band
                ntones = ((ovex->st[n].channels[chind].bandwidth - fabs (pcfreq_hz))
                        / (ovex->st[n].channels[chind].pcal_spacing)) + 1;
                                    // condition result
                ntones = (ntones > MAX_PCF) ? MAX_PCF : ntones;
                                    // go through list of all tones that are inband,
                                    // and see which ones we actually have pcal data for
                nin = 0;
                                    // preload mask for excluding particular tones
                mask = (stn == 0) ? pass->control.pc_tonemask[j].ref
                                  : pass->control.pc_tonemask[j].rem;
                for (i=0; i<ntones; i++)
                    {
                    for (k=0; k<MAX_PCF; k++)
                        if (fabs (pcfreq_hz - pass->pass_data[fr].pc_freqs[stn][k]) < 1e-6)
                            break;
                    
                    if (k<MAX_PCF && (mask & 1) == 0)
                        {
                        pass->pcinband[stn][fr][nin] = k;
                        msg ("adding pcinband[%d][%d][%d] %d pcfreq_hz %lf sb %c", 
                              0,stn, fr, nin, pass->pcinband[stn][fr][nin], 
                              pcfreq_hz, ovex->st[n].channels[chind].net_sideband);
                        nin++;
                        }
                                    // move on to next tone in the band
                    if (ovex->st[n].channels[chind].net_sideband == 'U')
                        pcfreq_hz += ovex->st[n].channels[chind].pcal_spacing;
                    else
                        pcfreq_hz -= ovex->st[n].channels[chind].pcal_spacing;
                    mask >>= 1;     // access next bit (i.e. tone) in mask
                    }
                                    // set rest of indices to -1 (unused)
                for (i=nin; i<MAX_PCF; i++)
                    pass->pcinband[stn][fr][i] = -1;
                                    // save maximum number of tones in any channel
                if (nin > pass->npctones)
                    pass->npctones = nin;
                }                   // end of multitone case
            }                       // end of fr loop
        }                           // end of stn loop

                                    // copy in ad hoc phase model
    param.ah_phase  = pass->control.adhoc_phase;
    param.ah_tref   = pass->control.adhoc_tref;
    param.ah_period = pass->control.adhoc_period;
    param.ah_amp    = pass->control.adhoc_amp * conrad;
    for (i=0; i<6; i++)
        param.ah_poly[i] = pass->control.adhoc_poly[i] * conrad;
    for (i=0; i<2; i++)
        {
        strcpy (param.ah_file[i], pass->control.adhoc_file[i]);
        strcpy (param.ah_file_chans[i], pass->control.adhoc_file_chans[i]);
        strcpy (param.ah_flag_files[i], pass->control.adhoc_flag_files[i]);
        strcpy (param.plot_data_dir[i], pass->control.plot_data_dir[i]);
        }

    if (do_accounting) account ("PreCorrect data");
    return (0);
    }
