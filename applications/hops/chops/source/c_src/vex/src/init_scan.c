/************************************************************************/
/*                                                                      */
/* This routine initializes the info structure, after it has been       */
/* suitably allocated.  This allows us to examine the filled in         */
/* structure after the fact for any critical missing information        */
/*                                                                      */
/*      Inputs:         scan            Pointer to the structure        */
/*                                                                      */
/*      Output:         scan            Duly initialized                */
/*                                                                      */
/* Created January 6 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

void
init_scan (struct scan_struct *scan,
           int nstat)
    {
    int i, j, k, l;
    struct station_struct *st;
    struct chan_struct *ch;
    
    scan->exper_num = I_UNDEFINED;
    scan->exper_name[0] = '\0';
    scan->correlator[0] = '\0';
    scan->scan_name[0] = '\0';
    clear_date (&(scan->start_time));
    clear_date (&(scan->ffit_reftime));
    scan->tai_utc = F_UNDEFINED;
    scan->a1_tai = F_UNDEFINED;
    scan->neop = I_UNDEFINED;
    clear_date (&(scan->eop_reftime));
    scan->eop_interval = I_UNDEFINED;
    for (i=0; i<10; i++)
        scan->ut1_utc[i] = scan->x_wobble[i] = scan->y_wobble[i] = F_UNDEFINED;

    scan->src.source_name[0] = '\0';
    scan->src.iau_name[0] = '\0';
    scan->src.source_type = I_UNDEFINED;
    scan->src.calibrator = I_UNDEFINED;
    scan->src.position.ra_hrs = I_UNDEFINED;
    scan->src.position.ra_mins = I_UNDEFINED;
    scan->src.position.ra_secs = F_UNDEFINED;
    scan->src.position.dec_degs = I_UNDEFINED;
    scan->src.position.dec_mins = I_UNDEFINED;
    scan->src.position.dec_secs = F_UNDEFINED;
    clear_date (&(scan->src.position_epoch));
    scan->src.position_ref_frame = I_UNDEFINED;
    scan->src.ra_rate = F_UNDEFINED;
    scan->src.dec_rate = F_UNDEFINED;

    scan->nst = nstat;
    for (i=0; i<nstat; i++)
        {
        st = scan->st + i;
        st->start_offset = I_UNDEFINED;
        st->stop_offset = I_UNDEFINED;
        st->start_tapepos = F_UNDEFINED;
        st->tape_motion = I_UNDEFINED;
        st->early_start = I_UNDEFINED;
        st->late_finish = I_UNDEFINED;
        st->tape_gap = I_UNDEFINED;
        st->subpass = C_UNDEFINED;
        st->passno = I_UNDEFINED;
        st->drive_no = I_UNDEFINED;
        st->site_type = I_UNDEFINED;
        st->site_name[0] = '\0';
        st->site_id[0] = '\0';
        st->mk4_site_id = C_UNDEFINED;
        for (j=0; j<3; j++) st->coordinates[j] = F_UNDEFINED;
        clear_date (&(st->coordinate_epoch));
        for (j=0; j<3; j++) st->site_velocity[j] = F_UNDEFINED;
        st->zenith_atm = F_UNDEFINED;
        st->occucode[0] = '\0';
        st->axis_type = I_UNDEFINED;
        st->axis_offset = F_UNDEFINED;
        st->recorder_type = I_UNDEFINED;
        st->rack_type = I_UNDEFINED;
        st->record_density = F_UNDEFINED;
        st->tape_length = F_UNDEFINED;
        st->recorder_id = I_UNDEFINED;
        st->clock_early = F_UNDEFINED;
        clear_date (&(st->clockrate_epoch));
        st->clockrate = F_UNDEFINED;
        st->tape_id[0] = '\0';
        st->samplerate = F_UNDEFINED;
        st->track_format = I_UNDEFINED;
        st->modulation = OFF;
        st->pass_direction = C_UNDEFINED;
        for (j=0; j<4; j++) st->head_position[j] = F_UNDEFINED;
        st->roll = OFF;
        st->roll_increment = I_UNDEFINED;
        st->roll_period = F_UNDEFINED;
        for (j=1; j<=4; j++)
            for (k=0; k<34; k++)
                for (l=0; l<32; l++) st->roll_seq[j][k][l] = I_UNDEFINED;
        for (j=0; j<16; j++)
            {
            ch = st->channels + j;
            ch->polarization = C_UNDEFINED;
            ch->sky_frequency = F_UNDEFINED;
            ch->net_sideband = C_UNDEFINED;
            ch->bandwidth = F_UNDEFINED;
            ch->band_id[0] = '\0';
            ch->chan_id[0] = '\0';
            ch->bbc_id[0] = '\0';
            ch->pcal_id[0] = '\0';
            ch->if_id[0] = '\0';
            ch->bbc_no = I_UNDEFINED;
            ch->if_name[0] = '\0';
            ch->if_total_lo = F_UNDEFINED;
            ch->if_sideband = C_UNDEFINED;
            ch->pcal_spacing = F_UNDEFINED;
            ch->pcal_base_freq = F_UNDEFINED;
            for (k=0; k<16; k++) ch->pcal_detect[k] = I_UNDEFINED;
            for (k=0; k<4; k++) ch->sign_tracks[k] = I_UNDEFINED;
            ch->sign_headstack = I_UNDEFINED;
            for (k=0; k<4; k++) ch->mag_tracks[k] = I_UNDEFINED;
            ch->mag_headstack = I_UNDEFINED;
            }
        }
    }
