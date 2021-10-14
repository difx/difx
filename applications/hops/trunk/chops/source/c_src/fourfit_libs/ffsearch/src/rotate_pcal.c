/*****************************************************************
* Subroutine to correct for phase-cal                            *
*                                                                *
*  version 1, August 1, 1991    - cmn                            *
*  modified for station-based phase cal treatment - rjc 93.5.6   *
*  added code for ad hoc phase corrections          rjc 94.9.6   *
*  inverted sign for ap_by_ap pcal for remote stn rjc 2008.10.31 *
*  save pcal phasor, rather than applying it      rjc 2011.12.21 *
*  fixed multitone phase sign bug                 rjc 2013.5.9   *
*  fixed index offset bug for ad hoc code         rjc 2018.4.12  *
*****************************************************************/
#include <stdio.h>
#include <math.h>
#include "hops_complex.h"
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"


rotate_pcal(struct type_pass *pass)
    {
    int ap, fr, i, ip;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    hops_complex rrpcal[2];
    double theta,
           phaze,thyme,thyme_n,zeta,
           deltaf,
           eta[4];
    struct data_corel *cor_data;
    struct interp_sdata *rrisd[2];
    extern struct type_status status;
    extern struct type_param param;
                                    // function prototype
    double diff_file_phase (struct type_pass *, char, double);


                                        /* This subroutine rotates to correct
                                           data for phasecal differences.
                                           If pcal is used each AP, it adjusts so
                                           that all APs have the
                                           same average phase_cal phase. */
    for (fr = 0; fr < pass->nfreq; fr++)
        {
                                        // calculate pc_delay-induced phase differences
        deltaf = 2e6 * M_PI * (pass->pass_data[fr].frequency - param.ref_freq);
        eta[0] = (pass->control.pc_delay_l.rem - pass->control.pc_delay_l.ref) * deltaf;
        eta[1] = (pass->control.pc_delay_r.rem - pass->control.pc_delay_r.ref) * deltaf;
        eta[2] = (pass->control.pc_delay_r.rem - pass->control.pc_delay_l.ref) * deltaf;
        eta[3] = (pass->control.pc_delay_l.rem - pass->control.pc_delay_r.ref) * deltaf;

        for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++)
            {
            if (param.ah_phase == SINEWAVE)    /* evaluate ad hoc phase model */
                {                       /* compute phase at center of AP */
                phaze = ((ap + 0.5) * param.acc_period + param.start - param.ah_tref)
                        / param.ah_period * 2.0 * M_PI;
                zeta = param.ah_amp * sin (phaze);
                }
            else if (param.ah_phase == POLYNOMIAL)
                {
                zeta = 0.0;
                thyme = (ap + 0.5) * param.acc_period + param.start - param.ah_tref;
                thyme_n = 1.0;
                for (i=0; i<6; i++)
                    {
                    zeta += param.ah_poly[i] * thyme_n;
                    thyme_n *= thyme;
                    }
                }
                                        // in adhoc file mode, find difference phase (f,t)
            else if (param.ah_phase == PHYLE)
                {
                thyme = ((ap + 0.5) * param.acc_period + param.start) / 8.64e4;
                zeta = diff_file_phase (pass, pass->pass_data[fr].freq_code, thyme);
                }
            else
                zeta = 0.0;             /* no ad hoc phase model */

            cor_data = &(pass->pass_data[fr].data[ap]);
            rrisd[0] = &(cor_data->ref_sdata);
            rrisd[1] = &(cor_data->rem_sdata);

                                        /* Any bit set implies data present */
            if (cor_data->flag == 0)
                continue;               // skip out to next ap

            for (ip=0; ip<4; ip++)      // loop over four possible pol products
                {
                theta = 0.0;
                for (i=0; i<2; i++)         // i index over ref:rem
                    {
                    switch (param.pc_mode[i])
                        {
                        case NORMAL:
                        case MANUAL:
                                        // apply constant pcal to whole scan
                            rrpcal[i] = cexp (I * status.pc_phase[fr][i][stnpol[i][ip]]);
                            break;
                        case AP_BY_AP:
                                        // form difference with correct pol
                            rrpcal[i] = (stnpol[i][ip]) ?
                                rrisd[i]->phasecal_rcp[pass->pci[i][fr]]:
                                rrisd[i]->phasecal_lcp[pass->pci[i][fr]];
                            rrpcal[i] = conj (rrpcal[i]);
                            break;
                        case MULTITONE:
                            rrpcal[i] = rrisd[i]->mt_pcal[stnpol[i][ip]];
                            break;
                        }
                    theta += (2*i-1) * carg (rrpcal[i]);
                    }
                                        // Zero pcal ampl => missing pcal data
                                        // so don't rotate
                if (cabs (rrpcal[0]) + cabs (rrpcal[1]) == 0.0)
                    theta = 0.0;
                                        // save resulting phasor in time-freq array
                cor_data->pc_phasor[ip] = cexp (I * (theta - zeta + eta[ip]));
                }
            }
        }
    }
