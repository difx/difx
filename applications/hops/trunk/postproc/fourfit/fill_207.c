/************************************************************************/
/*                                                                      */
/*  Fills in a type_207 record                                          */
/*                                                                      */
/*      Inputs:         via externs                                     */
/*                                                                      */
/*      Output:         t207        Filled in type_207 record           */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <math.h>
#include <stdio.h>
#include "mk4_data.h"
#include "vex.h"
#include "pass_struct.h"
#include "param_struct.h"

#define DEGRAD 180.0/M_PI

int
fill_207 (/* pass, status, param, t207) */
struct type_pass *pass,
struct type_status *status,
struct type_param *param,
struct type_207 *t207)
    {
    int i, j, pol;
    int stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L, 1:R
    float ref_errate, rem_errate, nreftrk, nremtrk;
    double midband;
    struct freq_corel *p;
    
    midband = 2.5e-4 / param->samp_period;  // midband freq in KHz
    clear_207 (t207);
                                        /* Phasecal information */
    t207->pcal_mode = 10 * param->pc_mode[0] + param->pc_mode[1];
    for (i=0; i<pass->nfreq; i++)
        {
        t207->ref_pcamp[i].lsb = status->pc_amp[i][0][stnpol[0][pass->pol]];
        t207->rem_pcamp[i].lsb = status->pc_amp[i][1][stnpol[1][pass->pol]];
        t207->ref_pcphase[i].lsb = status->pc_meas[i][0][stnpol[0][pass->pol]] * DEGRAD;
        t207->rem_pcphase[i].lsb = status->pc_meas[i][1][stnpol[1][pass->pol]] * DEGRAD;
        t207->ref_pcoffset[i].lsb = status->pc_offset[i][0][stnpol[0][pass->pol]];
        t207->rem_pcoffset[i].lsb = status->pc_offset[i][1][stnpol[1][pass->pol]];
        t207->ref_pcamp[i].usb = status->pc_amp[i][0][stnpol[0][pass->pol]];
        t207->rem_pcamp[i].usb = status->pc_amp[i][1][stnpol[1][pass->pol]];
        t207->ref_pcphase[i].usb = status->pc_meas[i][0][stnpol[0][pass->pol]] * DEGRAD;
        t207->rem_pcphase[i].usb = status->pc_meas[i][1][stnpol[1][pass->pol]] * DEGRAD;
        t207->ref_pcoffset[i].usb = status->pc_offset[i][0][stnpol[0][pass->pol]];
        t207->rem_pcoffset[i].usb = status->pc_offset[i][1][stnpol[1][pass->pol]];
                                        /* LSB unused for now  rjc 2001.6.19 */
        if (param->pc_mode[0] == MULTITONE)
            t207->ref_pcfreq[i].usb = midband;
        else
            t207->ref_pcfreq[i].usb = pass->pass_data[i].pc_freqs[0][pass->pci[0][i]];
        if (param->pc_mode[1] == MULTITONE)
            t207->rem_pcfreq[i].usb = midband;
        else
            t207->rem_pcfreq[i].usb = pass->pass_data[i].pc_freqs[1][pass->pci[1][i]];
        }

    t207->ref_pcrate = status->pc_rate[0];
    t207->rem_pcrate = status->pc_rate[1];
                                        /* Mean error rates, sidebands averaged */
    for (i=0; i<pass->nfreq; i++)
        {
        pol = pass->pol;
        p = pass->pass_data + i;
        nreftrk = nremtrk = 0.0;
        ref_errate = rem_errate = 0.0;
                                        /* Add up rates for all tracks */
        for (j=0; j<16; j++)
            {
            if ((pol == 0) || (pol == 2))
                if (p->trk_lcp[0][j] >= 0)
                    { ref_errate += p->mean_lcp_trk_err[0][j];nreftrk += 1.0; }
            if ((pol == 1) || (pol == 3))
                if (p->trk_rcp[0][j] >= 0)
                    { ref_errate += p->mean_rcp_trk_err[0][j];nreftrk += 1.0; }
            if ((pol == 0) || (pol == 3))
                if (p->trk_lcp[1][j] >= 0)
                    { rem_errate += p->mean_lcp_trk_err[1][j];nremtrk += 1.0; }
            if ((pol == 1) || (pol == 2))
                if (p->trk_rcp[1][j] >= 0)
                    { rem_errate += p->mean_rcp_trk_err[1][j];nremtrk += 1.0; }
            }
                                        /* Record arithmetic average */
        if (nreftrk > 0.0) 
            t207->ref_errate[i] = ref_errate / nreftrk;
        if (nremtrk > 0.0) 
            t207->rem_errate[i] = rem_errate / nremtrk;
        }

    return (0);
    }

