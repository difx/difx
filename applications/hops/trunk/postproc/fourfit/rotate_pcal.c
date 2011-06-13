/*****************************************************************
* Subroutine to correct for phase-cal                            *
*                                                                *
*  version 1, August 1, 1991    - cmn                            *
*  modified for station-based phase cal treatment - rjc 93.5.6   *
*  added code for ad hoc phase corrections          rjc 94.9.6   *
*  inverted sign for ap_by_ap pcal for remote stn rjc 2008.10.31 *
*****************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"


rotate_pcal(pass)
struct type_pass *pass;
    {
    int ap, fr, i;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    complex rrpcal[2], c_mult(), c_exp(), c_add();
    double theta, avg_pcal[2], c_phase(), c_mag(),
           phaze,thyme,thyme_n,zeta;
    struct data_corel *cor_data;
    struct interp_sdata *rrisd[2];
    extern struct type_status status;
    extern struct type_param param;

                                        /* This subroutine rotates to correct
                                           data for phasecal differences.
                                           If pcal is used each AP, it adjusts so
                                           that all APs have the
                                           same average phase_cal phase. */
    for (fr = 0; fr < pass->nfreq; fr++)
        {
        for (i=0; i<2; i++)
            avg_pcal[i] =  status.pc_offset[fr][i] * M_PI / 180.0
                         - status.pc_phase[fr][i];

        for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++)
            {
            cor_data = &(pass->pass_data[fr].data[ap]);
            rrisd[0] = &(cor_data->ref_sdata);
            rrisd[1] = &(cor_data->rem_sdata);

                                        /* Any bit set implies data present */
            if (cor_data->flag == 0) 
                continue;               // skip out to next ap

            theta = 0.0;
            for (i=0; i<2; i++)         // i index over ref:rem
                if (param.pc_mode[i] == AP_BY_AP)
                    {
                                        // FIXME - Add both polarizations for now
                    rrpcal[i] = c_add (rrisd[i]->phasecal_rcp[pass->pci[i][fr]],
                                       rrisd[i]->phasecal_lcp[pass->pci[i][fr]]);
                    theta += (2*i-1) * c_phase (rrpcal[i]) - avg_pcal[i];
                    }
                else if (param.pc_mode[i] == MULTITONE)
                    {
                    rrpcal[i] = rrisd[i]->mt_pcal[stnpol[i][param.pol]];
                    theta += (2*i-1) * (c_phase (rrpcal[i]) + avg_pcal[i]);
                    }
                                                                            
            if (param.ah_phase == SINEWAVE)    /* evaluate ad hoc phase model */
                {                       /* compute phase at center of AP */
                phaze = ((ap-0.5) * param.acc_period + param.start - param.ah_tref) 
                        / param.ah_period * 2.0 * M_PI;
                zeta = param.ah_amp * sin (phaze);
                }
            else if (param.ah_phase == POLYNOMIAL)
                {
                zeta = 0.0;
                thyme = (ap-1) * param.acc_period + param.start - param.ah_tref;
                thyme_n = 1.0;
                for (i=0; i<6; i++)
                    {
                    zeta += param.ah_poly[i] * thyme_n;
                    thyme_n *= thyme;
                    }
                }
            else
                zeta = 0.0;             /* no ad hoc phase model */

            if (param.pc_mode[0] == AP_BY_AP 
             || param.pc_mode[1] == AP_BY_AP
             || param.pc_mode[0] == MULTITONE
             || param.pc_mode[1] == MULTITONE
             || param.ah_phase != 0)
                {
                                        /* Zero pcal ampl => missing pcal data   
                                           so apply only mean pcal to this ap */
                if (c_mag (rrpcal[0]) + c_mag (rrpcal[1]) == 0.0) 
                    theta = 0.0;
                for (i = 0; i < param.nlags*2; i++) cor_data->sbdelay[i] = 
                                c_mult (cor_data->sbdelay[i], c_exp (theta - zeta));
                }
            }
        }
    }
