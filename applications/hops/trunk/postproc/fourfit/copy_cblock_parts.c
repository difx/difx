/*************************************************************************
*                                                                        *
*  copy_cblock_parts copies from one c_block into another, all those     *
*                    variables that have a non-null (i.e. valid) value   *
*                                                                        *
*                                                    RJC  92.8.31        *
*************************************************************************/
#include <string.h>
#include "control.h"
#include "mk4_sizes.h"


int
copy_cblock_parts (f,t)
struct c_block *f,*t;               // f (from) is source pointer
                                    // t (to)   is destination pointer
    {
    int i,j;

    if (f->max_parity != NULLFLOAT)
        t->max_parity = f->max_parity;

    if (f->pc_mode.ref != NULLINT)
        t->pc_mode.ref = f->pc_mode.ref;

    if (f->pc_mode.rem != NULLINT)
        t->pc_mode.rem = f->pc_mode.rem;

    if (f->pc_period.ref != NULLINT)
        t->pc_period.ref = f->pc_period.ref;

    if (f->pc_period.rem != NULLINT)
        t->pc_period.rem = f->pc_period.rem;

    if (f->lsb_offset.ref != NULLFLOAT)
        t->lsb_offset.ref = f->lsb_offset.ref;

    if (f->lsb_offset.rem != NULLFLOAT)
        t->lsb_offset.rem = f->lsb_offset.rem;

    if (f->skip != NULLINT)
        t->skip = f->skip;

    if (f->x_crc != NULLINT)
        t->x_crc = f->x_crc;

    if (f->y_crc != NULLINT)
        t->y_crc = f->y_crc;

    if (f->x_slip_sync != NULLINT)
        t->x_slip_sync = f->x_slip_sync;

    if (f->y_slip_sync != NULLINT)
        t->y_slip_sync = f->y_slip_sync;

    if (f->switched_mode != NULLINT)
        t->switched_mode = f->switched_mode;

    if (f->switched_period != NULLINT)
        t->switched_period = f->switched_period;

    if (f->use_samples != NULLINT)
        t->use_samples = f->use_samples;

    if (f->ref_freq != NULLFLOAT)
        t->ref_freq = f->ref_freq;

    if (f->ra_offset != NULLFLOAT)
        t->ra_offset = f->ra_offset;

    if (f->dec_offset != NULLFLOAT)
        t->dec_offset = f->dec_offset;

    if (f->adhoc_phase != NULLINT)
        t->adhoc_phase = f->adhoc_phase;

    if (f->adhoc_tref != NULLFLOAT)
        t->adhoc_tref = f->adhoc_tref;

    if (f->adhoc_period != NULLFLOAT)
        t->adhoc_period = f->adhoc_period;

    if (f->adhoc_amp != NULLFLOAT)
        t->adhoc_amp = f->adhoc_amp;

    if (f->ionosphere.ref != NULLFLOAT)
        t->ionosphere.ref = f->ionosphere.ref;

    if (f->ionosphere.rem != NULLFLOAT)
        t->ionosphere.rem = f->ionosphere.rem;

    if (f->t_cohere != NULLFLOAT)
        t->t_cohere = f->t_cohere;

    if (f->dc_block != NULLINT)
        t->dc_block = f->dc_block;

    if (f->optimize_closure != NULLINT)
        t->optimize_closure = f->optimize_closure;

    if (f->ion_npts != NULLINT)
        t->ion_npts = f->ion_npts;

    if (f->interpolator != NULLINT)
        t->interpolator = f->interpolator;

    if (f->station_delay.ref != NULLFLOAT)
        t->station_delay.ref = f->station_delay.ref;

    if (f->station_delay.rem != NULLFLOAT)
        t->station_delay.rem = f->station_delay.rem;

    if (f->pc_delay_l.ref != NULLFLOAT)
        t->pc_delay_l.ref = f->pc_delay_l.ref;

    if (f->pc_delay_l.rem != NULLFLOAT)
        t->pc_delay_l.rem = f->pc_delay_l.rem;

    if (f->pc_delay_r.ref != NULLFLOAT)
        t->pc_delay_r.ref = f->pc_delay_r.ref;

    if (f->pc_delay_r.rem != NULLFLOAT)
        t->pc_delay_r.rem = f->pc_delay_r.rem;

    if (f->weak_channel != NULLFLOAT)
        t->weak_channel = f->weak_channel;

    if (f->pc_amp_hcode != NULLFLOAT)
        t->pc_amp_hcode = f->pc_amp_hcode;

    if (f->fmatch_bw_pct != NULLFLOAT)
        t->fmatch_bw_pct = f->fmatch_bw_pct;

    if (f->nsamplers != NULLINT)
        {
        t->nsamplers = f->nsamplers;
        for (i=0; i<f->nsamplers; i++)
            t->psamplers[i] = f->psamplers[i];
        strcpy (t->sampler_codes, f->sampler_codes);
        }

    if (f->adhoc_poly[0] != NULLFLOAT)
        for (i=0; i<6; i++)
            if (f->adhoc_poly[i] != NULLFLOAT)
                t->adhoc_poly[i] = f->adhoc_poly[i];
            else                    // pad extant poly. with high order zero terms
                t->adhoc_poly[i] = 0.0;

    for (i=0; i<2; i++)
        {
        if (f->sb_window[i] != NULLFLOAT)
            t->sb_window[i] = f->sb_window[i];

        if (f->mb_window[i] != NULLFLOAT)
            t->mb_window[i] = f->mb_window[i];

        if (f->dr_window[i] != NULLFLOAT)
            t->dr_window[i] = f->dr_window[i];

        if (f->time_span[i] != NULLINT)
            t->time_span[i] = f->time_span[i];

        if (f->passband[i] != NULLFLOAT)
            t->passband[i] = f->passband[i];

        if (f->ion_window[i] != NULLFLOAT)
            t->ion_window[i] = f->ion_window[i];
        }


    if (f->index[0] != NULLINT)     // this needs work (deprecated)
        for (i=0; i<32; i++)
            t->index[i] = f->index[i];

                                    // only sidebands that are present in both the from-list
                                    // and the to-list are present in the resulting to-list
    for (i=0; i<MAX_CHAN_PP; i++)                         
        {
        if (f->frequency[i] != NULLINT)
            t->frequency[i] &= f->frequency[i];

        if (t->frequency[i])        // copy iff this freq. still active
            {
            for (j=0; j<2; j++)     // copy phases for both polarizations
                {
                if (f->pc_phase[i][j].ref != NULLFLOAT)
                    t->pc_phase[i][j].ref = f->pc_phase[i][j].ref;

                if (f->pc_phase[i][j].rem != NULLFLOAT)
                    t->pc_phase[i][j].rem = f->pc_phase[i][j].rem;
                }

            if (f->pc_freq[i].ref != NULLFLOAT)
                t->pc_freq[i].ref = f->pc_freq[i].ref;

            if (f->pc_freq[i].rem != NULLFLOAT)
                t->pc_freq[i].rem = f->pc_freq[i].rem;

            if (f->pc_tonemask[i].ref != NULLINT)
                t->pc_tonemask[i].ref = f->pc_tonemask[i].ref;

            if (f->pc_tonemask[i].rem != NULLINT)
                t->pc_tonemask[i].rem = f->pc_tonemask[i].rem;

            if (f->gates[i].on_delay != NULLINT)
                t->gates[i].on_delay = f->gates[i].on_delay;

            if (f->gates[i].duration != NULLINT)
                t->gates[i].duration = f->gates[i].duration;

            if (f->delay_offs[i].ref != NULLFLOAT)
                t->delay_offs[i].ref = f->delay_offs[i].ref;

            if (f->delay_offs[i].rem != NULLFLOAT)
                t->delay_offs[i].rem = f->delay_offs[i].rem;
            }
        }

    return(0);
    }
