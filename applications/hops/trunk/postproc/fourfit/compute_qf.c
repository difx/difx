/************************************************************************/
/*                                                                      */
/* Computes Mk4-style quality code and error code, replacing single     */
/* Mk3 quality factor.  Also generates so-called "tape qcode" because   */
/* A-file based software will probably need it for a while after the    */
/* transition to Mk4.                                                   */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <complex.h>
#include <string.h>
#include "mk4_data.h"
#include "pass_struct.h"
#include "param_struct.h"
#include "control.h"
#include "statistics.h"
#include "filter.h"


int
compute_qf (
struct type_pass *pass,
struct type_param *param,
struct type_status *status,
char *qcode, 
char *errcode, 
char *tape_qcode)
    {
    int i, qf, missing_track, low_chan, low_pcal[2], e_code;
    int t1000_index, btable_index, fr, min, max, num_ap, ratio, perr;
    int slip_measure, slip_pct;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    double diff1, diff2, intg_time, dur, fract;
    extern struct type_statistics statistics;
    extern struct type_filter filter;

                                        /* Figure out a few conditions for */
                                        /* calculation of qcode */
                                        /* Missing tracks for D-code */
    missing_track = FALSE;
                                        /* Channel < half of mean for 2-code */
    low_chan = FALSE;
                                        /* Low Pcal amplitude < some threshhold */
    low_pcal[0] = low_pcal[1] = FALSE;
    for (i=0; i<pass->nfreq; i++)
        {
                                        /* Both sidebands must be absent to 
                                           cause a D-code */
        if ((status->ap_num[0][i] == 0) && (status->ap_num[1][i] == 0))
            missing_track = TRUE;
        if (cabs (status->fringe[i]) < (param->weak_channel * status->inc_avg_amp_freq))
            low_chan = TRUE;
                                        /* re-enable the following test;
                                         * change threshold units  rjc 2001.10.25 
                                         * also mark high amp bad  rjc 2005.10.26 */
        if (status->pc_amp[i][0][stnpol[0][pass->pol]] < param->pc_amp_hcode
         || status->pc_amp[i][0][stnpol[0][pass->pol]] > 0.500) 
            low_pcal[0] = TRUE;
        if (status->pc_amp[i][1][stnpol[1][pass->pol]] < param->pc_amp_hcode
         || status->pc_amp[i][1][stnpol[1][pass->pol]] > 0.500) 
            low_pcal[1] = TRUE;
        }
                                        /* Zero-width windows nullify B/E-codes */
    if (param->win_sb[0] == param->win_sb[1]) 
        status->interp_err &= ~(WIN_EDGE_SBD | INTP_ERR_SBD);
    if (param->win_mb[0] == param->win_mb[1])
        status->interp_err &= ~(WIN_EDGE_MBD | INTP_ERR_MBD);
    if (param->win_dr[0] == param->win_dr[1])
        status->interp_err &= ~(WIN_EDGE_RATE | INTP_ERR_RATE);
                                        /* Default value */
    *errcode = ' ';
                                        /* Fill in qcode ... earlier codes in this */
                                        /* if/else clause override later codes */
                                        /* A-code means fourfit unable to handle */
                                        /* (probably will never implement) */
    if (0==1) 
        *errcode = 'A';
                                        /* B-code caused by interpolation error, */
                                        /* usually due to fringes at edge of window */
    else if (status->interp_err & (INTP_ERR_SBD | INTP_ERR_MBD | INTP_ERR_RATE))
        {
        msg ("Interpolation error", 2);
        *errcode = 'B';
        }
                                        /* C epoch error condition test here */
    else if (0==1) 
        *errcode = 'C';
                                        /* D-code, at least 1 channel missing */
    else if (missing_track)
        *errcode = 'D';
                                        /* F code means no data found, dummy output */
                                        /* (not yet implemented) */
                                        /* for now, use F to catch SU "forks"
                                         * rjc 2001.2.26 */
    else if (((filter.zero[0] - pass->nfreq) * 4 >= status->total_ap) 
          || ((filter.zero[0] - pass->nfreq) * 4 >= status->total_ap)) 
        *errcode = 'F';
                                        /* No fringes, just leave at 0. Placed */
                                        /* here to override E, 1 and 2 codes */
    else if (status->prob_false > 1.E-4)
        ;
                                        /* E code means solution at edge of */
                                        /* window, see above (may need more */
                                        /* sophistication for wide-open windows */
    else if (status->interp_err & (WIN_EDGE_SBD | WIN_EDGE_MBD | WIN_EDGE_RATE))
        *errcode = 'E';
                                        /* G-code means a weak channel when SNR>20 */
    else if (low_chan && (status->snr > 20.0))
        *errcode = 'G';
                                        /* H-code means 1 or more pcals < .01 */
                                        /* when in normal pcal mode */
                                        /* When in multione mode, this means */
                                        /* that the coherent avarage pcal amp */
                                        /* is below the threshold. However, it*/
                                        /* is still possible for individual tones */
                                        /* to be below the threhold, and not have */
                                        /* and H code flagged */
    else if ( (low_pcal[0] && 
              (param->pc_mode[0] == NORMAL || param->pc_mode[0] == MULTITONE))||  
              (low_pcal[1] && 
              (param->pc_mode[1] == NORMAL || param->pc_mode[1] == MULTITONE)) )
        *errcode = 'H';
                                        /* Numeric quality codes */
                                        /* quality factor starts at 9, and falls */
                                        /* as more faults are found in the fit */
    *qcode = '9';
    if (status->timerms_phase > 11.46 && status->th_timerms_phase < 5.73)
        (*qcode)--;

    if (status->timerms_phase > 22.92 && status->th_timerms_phase < 11.46)
        (*qcode)--;

    if (status->freqrms_phase > 11.46 && status->th_freqrms_phase < 5.73)
        (*qcode)--;

    if (status->freqrms_phase > 22.92 && status->th_freqrms_phase < 11.46)
        (*qcode)--;

    if (status->timerms_amp > 20.0 && status->th_timerms_amp < 10.0)
        (*qcode)--;

    if (status->timerms_amp > 40.0 && status->th_timerms_amp < 20.0)
        (*qcode)--;

    if (status->freqrms_amp > 20.0 && status->th_freqrms_amp < 10.0)
        (*qcode)--;

    if (status->freqrms_amp > 40.0 && status->th_freqrms_amp < 20.0)
        (*qcode)--;

    if (*qcode < '1') *qcode = '1';
                                        /* No fringes, 0-code */
    if (status->prob_false > 1.E-4) *qcode = '0';
                                        /* Tape qcode requires a bit of effort ... */
    strncpy (tape_qcode, "??????", 6);
                                        /* overall parity error rate exponents */
    if (statistics.xperror < 1.E-9)
        perr =9;
    else
        perr = (int)(-log10 (statistics.xperror));
    tape_qcode[0] = '0' + perr;

    if (statistics.yperror < 1.E-9)
        perr =9;
    else
        perr = (int)(-log10 (statistics.yperror));
    tape_qcode[3] = '0' + perr;
                                        /* Slip sync rates */
    /*  slip_pct = (int)(100.0 * statistics.xslip);  */
    slip_pct = 0;                       /* rjc 2001.2.27 */
    slip_measure = 9 - slip_pct;
    if (slip_measure < 0) slip_measure = 0;
    tape_qcode[1] = '0' + slip_measure;
    /* slip_pct = (int)(100.0 * statistics.yslip); */
    slip_pct = 0;
    slip_measure = 9 - slip_pct;
    if (slip_measure < 0) slip_measure = 0;
    tape_qcode[4] = '0' + slip_measure;

                                        /* Ratio of min to max aps in a channel (QB) */
    min = max = status->ap_num[0][0] + status->ap_num[1][0];
    for (fr = 1; fr < pass->nfreq; fr++)
        {
        num_ap = status->ap_num[0][fr] + status->ap_num[1][fr];
        if (num_ap > max) max = num_ap;
        if (num_ap < min) min = num_ap;
        }
    if (max > 0) ratio = (10 * min) / max;
    if (ratio == 10) ratio = 9;
    tape_qcode[2] = '0' + ratio;

    return (0);
    }
                                        /* Fraction of data processed ... */
                                        /* Nominal scan duration */
/*     t1000_index = (baseline - 1) / 8; */
/*     btable_index = (baseline - 1) % 8; */
/*     dur = root->t1000[t1000_index].barray[btable_index].bduration; */
/*     if (dur == 0) dur = root->t1000[0].duration;   /* duration same on all baselines */
/*     intg_time = status->total_ap * param->acc_period / pass->channels; */
/*     fract = 10.0 * intg_time / dur; */
/*     if (fract == 10.0) fract = 9.9; */
/*     t4300->tape_qcode[5] = '0' + (int)(floor (fract)); */
