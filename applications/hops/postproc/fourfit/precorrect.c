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
#include "msg.h"
#include "mk4_data.h"
#include "mk4_util.h"
#include "ovex.h"
#include "pass_struct.h"
#include "param_struct.h"
#include "control.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "hops_complex.h"
#include "ffcontrol.h"
//#include "ff_misc_if.h"

int precorrect (struct scan_struct* ovex, struct type_pass* pass)
    {
    int xx, ii, jj, kt, stn, fr, ntones, nin, mask, chind;
    double delay_offset, rate_offset, pcfreq_hz,
           pcphas[2][2],            // indexed by [stn][pol]
           pcfreq[2],
           static_pc_off;
    extern struct type_status status;
    extern struct type_param param;
    extern int do_accounting;
    static double conrad = 0.01745329252; // mult. for deg to radians

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

    param.mixed_mode_rot = pass->control.mixed_mode_rot;

    param.noautofringes = pass->control.noautofringes;
    param.mod4numbering = pass->control.mod4numbering;
    param.polfringnames = pass->control.polfringnames;
    for (ii=0; ii<3; ii++)
        param.mbdrplopt[ii] = pass->control.mbdrplopt[ii];

    // Copy windows into working area
    for (ii=0; ii<2; ii++)
        {
        param.win_sb[ii] = pass->control.sb_window[ii] + delay_offset;
        param.win_mb[ii] = pass->control.mb_window[ii] + delay_offset;
        param.win_dr[ii] = pass->control.dr_window[ii] + rate_offset;
        param.passband[ii] = pass->control.passband[ii];
        param.avxpzoom[ii] = pass->control.avxpzoom[ii];
        param.avxplopt[ii] = pass->control.avxplopt[ii];
                                    // ionosphere window is about differential a priori
                                    // collapses if only 1 pt (i.e. no search)
        param.win_ion[ii] = (param.ion_pts > 1) ?
            pass->control.ion_window[ii] : 0.0;
        param.win_ion[ii] += (pass->control.ionosphere.rem == NULLFLOAT) ?
                           0.0 : pass->control.ionosphere.rem;
        param.win_ion[ii] -= (pass->control.ionosphere.ref == NULLFLOAT) ?
                           0.0 : pass->control.ionosphere.ref;
        
        param.mount_type[ii] = pass->control.mount_type[ii];
        }
    param.nnotches = pass->control.nnotches;
    for (ii=0; ii<pass->control.nnotches; ii++)
        {
        param.notches[ii][0] = pass->control.notches[ii][0];
        param.notches[ii][1] = pass->control.notches[ii][1];
        param.chan_notches[ii] = pass->control.chan_notches[ii];
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
    for (stn=0; stn<2; stn++)       // ref == 0, rem == 1
        {
        struct station_struct *ovex_st_n = &ovex->st[param.ov_bline[stn]];
        // n is (struct scan_struct) *ovex file mk4_site_id for ref/rem station
        // n = param.ov_bline[stn];
        // ovex->st[n] is the station structure for the ref/rem station
        // channels[0] is used as all channels are required to have same spacing
        param.pcal_spacing[stn] = ovex_st_n->channels[0].pcal_spacing;

        for (fr = 0; fr < pass->nfreq; fr++)
            {
            // jj = fcode(pass->pass_data[fr].freq_code, pass->control.chid);
            jj = pass->pass_data[fr].fcode_index;
            // See around line 280 of make_passes().
            chind = pass->pass_data[fr].ch_idx[stn];

                                    // copy delay calib. values into status array
            status.delay_offs[fr][stn] = (stn) ? pass->control.delay_offs[jj].rem
                                               : pass->control.delay_offs[jj].ref;

            if (param.pc_mode[stn] != MULTITONE)
                {                   // single tone used in this frequency band, process it
                                    // find corresponding freq index in control structure
                msg("non-multitone precorrect for freq_code %c station %s ch index %d", 1,
                    pass->pass_data[fr].freq_code, (stn==0)?"ref":"rem", chind);
                if (stn == 0)
                    {
                    for (xx=0; xx<2; xx++)  // xx = LXH or RYV
                        pcphas[stn][xx] = pass->control.pc_phase[jj][xx].ref;
                    pcfreq[stn] = pass->control.pc_freq[jj].ref;
                    }
                else
                    {
                    for (xx=0; xx<2; xx++)
                        pcphas[stn][xx] = pass->control.pc_phase[jj][xx].rem;
                    pcfreq[stn] = pass->control.pc_freq[jj].rem;
                    }

                for (xx=0; xx<2; xx++)
                    {
                    msg("pcphas[%d][%d] = %+.3f", 1, stn, xx, pcphas[stn][xx]);
                    status.pc_offset[fr][stn][xx] = (pcphas[stn][xx] != NULLFLOAT) ? pcphas[stn][xx] : 0.0;
                                    //add the static (constant across all channels) phase offset for each pol
                    static_pc_off = (stn == 0)
                                  ? pass->control.pc_phase_offset[xx].ref
                                  : pass->control.pc_phase_offset[xx].rem;
                                  
                    //is mixed-mode extra 90 deg rotation turned on?
                    if(param.mixed_mode_rot)
                    {
                        //this is a mixed mode (circular pol. x linear pol.) experiment?
                        if( pass->linpol[0] != pass->linpol[1] )
                        {
                            if(pass->linpol[stn] == xx && xx == 1 )
                            {   //apply the rotation to the lin-pol station only
                                msg ("adding -90 deg to static pc_offset for RY/YR data of station %c", 1,
                                    ovex_st_n->mk4_site_id);
                                static_pc_off += -90.0;
                            }
                        }
                    }

                    status.pc_offset[fr][stn][xx] += (static_pc_off != NULLFLOAT) ? static_pc_off : 0.0;
                    msg("pc_offset[%d][%d][%d] = %+.3f (fc %c at %d)", 1,
                        fr, stn, xx, status.pc_offset[fr][stn][xx], pass->pass_data[fr].freq_code, jj);
                    }

                                    // expand tone #'s into frequencies, if necessary
                if (fabs (pcfreq[stn]) > 64)
                    pcfreq_hz = 1e3 * pcfreq[stn];

                else                // specified as tone #, rather than freq
                    {               // must compute frequency for this tone
                                    // assume for now that all ovex channels the same spacing
                    pcfreq_hz = fmod (pass->pass_data[fr].frequency * 1e6
                                    - ovex_st_n->channels[chind].pcal_base_freq,
                                      ovex_st_n->channels[chind].pcal_spacing);

                                    // pcfreq_hz is positive distance from next lower line
                    if (pcfreq_hz < 0.0)
                        pcfreq_hz += ovex_st_n->channels[chind].pcal_spacing;

                                    // nearest freq rail depends on sideband
                    if (ovex_st_n->channels[chind].net_sideband == 'U')
                        {               // distance to next line is complement
                        pcfreq_hz = ovex_st_n->channels[chind].pcal_spacing - pcfreq_hz;
                                    // now set freq to tone # within band (1 relative)
                        pcfreq_hz += (pcfreq[stn] - 1) * ovex_st_n->channels[chind].pcal_spacing;
                        }
                    else            // set up tone frequency for net LSB
                        {           // lsb implemented using negative frequencies
                                    // now set freq to tone # within band (1 relative)
                        pcfreq_hz = - pcfreq_hz
                                    + (pcfreq[stn] + 1) * ovex_st_n->channels[chind].pcal_spacing;
                        }
                    msg ("fcode %c freq %lf tone request %lf pc_freqhz %lf", 0,
                          pass->pass_data[fr].freq_code, pass->pass_data[fr].frequency,
                          pcfreq[stn], pcfreq_hz);
                    }

                for (kt=0; kt<MAX_PCF; kt++)
                    if (fabs (pcfreq_hz - pass->pass_data[fr].pc_freqs[stn][kt]) < 1e-6)
                        break;
                    else
                        msg ("pcal freq %lf didn't match", -2,
                             pass->pass_data[fr].pc_freqs[stn][kt]);
                                    // if requested frequency not available,
                                    // complain about it and use the 1st tone
                if (kt == MAX_PCF)
                    {
                    kt = 0;
                    pcfreq_hz = pass->pass_data[fr].pc_freqs[stn][kt];
                    if (param.pc_mode[stn] != MANUAL)
                        msg ("stn %d pcal tone of %g Hz unavailable for channel %c",
                         1, stn, pcfreq_hz, pass->pass_data[fr].freq_code);
                    }
                pass->pci[stn][fr] = kt;
                pass->pcinband[stn][fr][kt] = kt;
                msg ("stn %d using pcal tone #%d of %lf Hz for freq %d code %c",
                     0, stn, kt, pcfreq_hz, fr, pass->pass_data[fr].freq_code);
                }                   // end of non-multitone mode case
            else
                {                   // process all tones in multitone mode
                for (xx=0; xx<2; xx++)
                    pcphas[stn][xx] = (stn == 0) ? pass->control.pc_phase[jj][xx].ref
                                                 : pass->control.pc_phase[jj][xx].rem;
                                    // apply a priori phase offset to each pol
                for (xx=0; xx<2; xx++)
                {
                    status.pc_offset[fr][stn][xx] =
                        (pcphas[stn][xx] != NULLFLOAT) ? pcphas[stn][xx]
                                                       : 0.0;
                                    //add the static (constant across all channels) phase offset for each pol
                    static_pc_off = (stn == 0)
                                  ? pass->control.pc_phase_offset[xx].ref
                                  : pass->control.pc_phase_offset[xx].rem;

                    //is mixed-mode extra 90 deg rotation turned on?
                    if(param.mixed_mode_rot)
                    {
                        //this is a mixed mode (circular pol. x linear pol.) experiment?
                        if( pass->linpol[0] != pass->linpol[1] )
                        {
                            if(pass->linpol[stn] == 1 && xx == 1 )
                            {   //apply the rotation to the lin-pol station only
                                msg ("adding -90 deg to static pc_offset for RY/YR data of station %c", 1, ovex_st_n->mk4_site_id);
                                static_pc_off += -90.0;
                            }
                        }
                    }

                    status.pc_offset[fr][stn][xx] += (static_pc_off != NULLFLOAT) ? static_pc_off
                                                                                  : 0.0;
                }

                                    // assume for now that all ovex channels the same spacing
                pcfreq_hz = fmod (pass->pass_data[fr].frequency * 1e6
                                - ovex_st_n->channels[chind].pcal_base_freq,
                                  ovex_st_n->channels[chind].pcal_spacing);

                                    // pcfreq_hz is positive distance from next lower line
                if (pcfreq_hz <= 0.0)
                    pcfreq_hz += ovex_st_n->channels[chind].pcal_spacing;
                                    // nearest freq rail depends on sideband
                if (ovex_st_n->channels[chind].net_sideband == 'U')
                                    // for USB, distance to next line is complement
                    pcfreq_hz = ovex_st_n->channels[chind].pcal_spacing - pcfreq_hz;
                                    // set up tone frequency for net LSB
                else                // lsb implemented using negative frequencies
                    pcfreq_hz = - pcfreq_hz;
                                    // calculate the number of tones in the band
                ntones = ((ovex_st_n->channels[chind].bandwidth - fabs (pcfreq_hz))
                        / (ovex_st_n->channels[chind].pcal_spacing)) + 1;
                                    // condition result
                ntones = (ntones > MAX_PCF) ? MAX_PCF : ntones;
                                    // go through list of all tones that are inband,
                                    // and see which ones we actually have pcal data for
                nin = 0;
                                    // preload mask for excluding particular tones
                mask = (stn == 0) ? pass->control.pc_tonemask[jj].ref
                                  : pass->control.pc_tonemask[jj].rem;
                for (ii=0; ii<ntones; ii++)
                    {
                    for (kt=0; kt<MAX_PCF; kt++)
                        if (fabs (pcfreq_hz - pass->pass_data[fr].pc_freqs[stn][kt]) < 1e-6)
                            break;

                    if (kt<MAX_PCF && (mask & 1) == 0)
                        {
                        pass->pcinband[stn][fr][nin] = kt;
                        msg ("adding pcinband[%d][%d][%d] %d pcfreq_hz %lf sb %c",
                              0,stn, fr, nin, pass->pcinband[stn][fr][nin],
                              pcfreq_hz, ovex_st_n->channels[chind].net_sideband);
                        nin++;
                        }
                                    // move on to next tone in the band
                    if (ovex_st_n->channels[chind].net_sideband == 'U')
                        pcfreq_hz += ovex_st_n->channels[chind].pcal_spacing;
                    else
                        pcfreq_hz -= ovex_st_n->channels[chind].pcal_spacing;
                    mask >>= 1;     // access next bit (i.e. tone) in mask
                    }
                                    // set rest of indices to -1 (unused)
                for (ii=nin; ii<MAX_PCF; ii++)
                    pass->pcinband[stn][fr][ii] = -1;
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
    for (ii=0; ii<6; ii++)
        param.ah_poly[ii] = pass->control.adhoc_poly[ii] * conrad;
    for (ii=0; ii<2; ii++)
        {
        strcpy (param.ah_file[ii], pass->control.adhoc_file[ii]);
        strcpy (param.ah_file_chans[ii], pass->control.adhoc_file_chans[ii]);
        strcpy (param.ah_flag_files[ii], pass->control.adhoc_flag_files[ii]);
        strcpy (param.plot_data_dir[ii], pass->control.plot_data_dir[ii]);
        strcpy(param.clones[ii], pass->control.clones[ii]);
        }
    param.clone_snr_chk = pass->control.clone_snr_chk;

    if (do_accounting) account ("PreCorrect data");
    return (0);
    }
