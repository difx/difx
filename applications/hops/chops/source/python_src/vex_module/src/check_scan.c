/************************************************************************/
/*                                                                      */
/* This routine makes sure the filled in structure is filled in enough  */
/* and in its initial incarnation just prints out the contents          */
/*                                                                      */
/*      Inputs:         scan            Pointer to the structure        */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created January 6 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

void
prt_date (char *string,
          struct date *date)
    {
    printf ("%s = %02dy%03dd%02dh%02dm%fs\n", string, date->year, date->day, date->hour,
                        date->minute, date->second);
    }

void
check_scan (/* scan */
struct scan_struct *scan)
    {
    int i, j, k, l;
    struct station_struct *st;
    struct chan_struct *ch;

    if (scan == NULL)
        {
        msg ("Invalid scan_struct pointer in check_scan()", 2);
        return;
        }
    
    printf ("exper_num = %d\n", scan->exper_num);
    printf ("exper_name = %s\n", scan->exper_name);
    printf ("correlator = %s\n", scan->correlator);
    printf ("scan_name = %s\n", scan->scan_name);
    prt_date ("start_time", &(scan->start_time));
    printf ("tai_utc = %g\n", scan->tai_utc);
    printf ("a1_tai = %g\n", scan->a1_tai);
    printf ("neop = %d\n", scan->neop);
    prt_date ("eop_reftime", &(scan->eop_reftime));
    printf ("eop_interval = %d\n", scan->eop_interval);
    for (i=0; i<scan->neop; i++)
        {
        if (scan->ut1_utc[i] != F_UNDEFINED) 
                printf ("ut1_utc[%d] = %g\n", i, scan->ut1_utc[i]);
        if (scan->x_wobble[i] != F_UNDEFINED) 
                printf ("x_wobble[%d] = %g\n", i, scan->x_wobble[i]);
        if (scan->y_wobble[i] != F_UNDEFINED) 
                printf ("y_wobble[%d] = %g\n", i, scan->y_wobble[i]);
        }

    printf ("source_name = %s\n", scan->src.source_name);
    printf ("iau_name = %s\n", scan->src.iau_name);
    printf ("source_type = %d\n", scan->src.source_type);
    printf ("calibrator = %d\n", scan->src.calibrator);
    printf ("pos1 = %d\n", scan->src.position.ra_hrs);
    printf ("pos2 = %d\n", scan->src.position.ra_mins);
    printf ("pos3 = %g\n", scan->src.position.ra_secs);
    printf ("pos4 = %d\n", scan->src.position.dec_degs);
    printf ("pos5 = %d\n", scan->src.position.dec_mins);
    printf ("pos6 = %g\n", scan->src.position.dec_secs);
    prt_date ("position_epoch", &(scan->src.position_epoch));
    printf ("pos_ref_frame = %d\n", scan->src.position_ref_frame);
    printf ("ra_rate = %g\n", scan->src.ra_rate);
    printf ("dec_rate = %g\n", scan->src.dec_rate);

    printf ("nst = %d\n", scan->nst);
    for (i=0; i<scan->nst; i++)
        {
        st = scan->st + i;
        printf ("start_offset = %d\n", st->start_offset);
        printf ("stop_offset = %d\n", st->stop_offset);
        printf ("start_tapepos = %g\n", st->start_tapepos);
        printf ("tape_motion = %d\n", st->tape_motion);
        printf ("early_start = %d\n", st->early_start);
        printf ("late_finish = %d\n", st->late_finish);
        printf ("tape_gap = %d\n", st->tape_gap);
        printf ("subpass = %c\n", st->subpass);
        printf ("passno = %d\n", st->passno);
        printf ("drive_no = %d\n", st->drive_no);
        printf ("site_type = %d\n", st->site_type);
        printf ("site_name = %s\n", st->site_name);
        printf ("site_id = %s\n", st->site_id);
        for (j=0; j<3; j++) printf ("coordinates[%d] = %g\n", j, st->coordinates[j]);
        prt_date ("coordinate_epoch", &(st->coordinate_epoch));
        for (j=0; j<3; j++)  printf ("site_velocity[%d] = %g\n", j, st->site_velocity[j]);
        printf ("zenith_atm = %g\n", st->zenith_atm);
        printf ("occucode = %s\n", st->occucode);
        printf ("axis_type = %d\n", st->axis_type);
        printf ("axis_offset = %g\n", st->axis_offset);
        printf ("recorder_type = %d\n", st->recorder_type);
        printf ("rack_type = %d\n", st->rack_type);
        printf ("record_density = %g\n", st->record_density);
        printf ("tape_length = %g\n", st->tape_length);
        printf ("recorder_id = %d\n", st->recorder_id);
        printf ("clock_early = %g\n", st->clock_early);
        prt_date ("clockrate_epoch", &(st->clockrate_epoch));
        printf ("clockrate = %g\n", st->clockrate);
        printf ("tape_id = %s\n", st->tape_id);
        printf ("samplerate = %g\n", st->samplerate);
        printf ("track_format = %d\n", st->track_format);
        printf ("modulation = %d\n", st->modulation);
        printf ("pass_direction = %c\n", st->pass_direction);
        for (j=0; j<4; j++) printf ("head_position[%d] = %g\n", j, st->head_position[j]);
        printf ("roll = %d\n", st->roll);
        printf ("roll_increment = %d\n", st->roll_increment);
        printf ("roll_period = %f\n", st->roll_period);
        for (j=1; j<=4; j++)
            for (k=2; k<34; k++)
                {
                if (st->roll_seq[j][k][0] == I_UNDEFINED) continue;
                printf ("roll_seq, hs %d: home %2d:", j, k);
                for (l=0; l<32; l++)
                    {
                    if (st->roll_seq[j][k][l] != I_UNDEFINED) 
                        printf (" %2d", st->roll_seq[j][k][l]);
                    }
                printf ("\n");
                }
        for (j=0; j<16; j++)
            {
            ch = st->channels + j;
                                        /* Skip if freq undefined */
            if (ch->sky_frequency == F_UNDEFINED) break;
            printf ("Channel %d\n", j);
            printf ("polarization = %c\n", ch->polarization);
            printf ("sky_frequency = %g\n", ch->sky_frequency);
            printf ("net_sideband = %c\n", ch->net_sideband);
            printf ("bandwidth = %g\n", ch->bandwidth);
            printf ("band_id = %s\n", ch->band_id);
            printf ("chan_id = %s\n", ch->chan_id);
            printf ("bbc_id = %s\n", ch->bbc_id);
            printf ("pcal_id = %s\n", ch->pcal_id);
            printf ("if_id = %s\n", ch->if_id);
            printf ("bbc_no = %d\n", ch->bbc_no);
            printf ("if_name = %s\n", ch->if_name);
            printf ("if_total_lo = %g\n", ch->if_total_lo);
            printf ("if_sideband = %c\n", ch->if_sideband);
            printf ("pcal_spacing = %g\n", ch->pcal_spacing);
            printf ("pcal_base_freq = %g\n", ch->pcal_base_freq);
            for (k=0; k<8; k++) printf ("pcal_detect[%d] = %d\n", k, ch->pcal_detect[k]);
            for (k=0; k<4; k++) printf ("sign_tracks[%d] = %d\n", k, ch->sign_tracks[k]);
            printf ("sign_headstack = %d\n", ch->sign_headstack);
            for (k=0; k<4; k++) printf ("mag_tracks[%d] = %d\n", k, ch->mag_tracks[k]);
            printf ("mag_headstack = %d\n", ch->mag_headstack);
            }
        }
    }
