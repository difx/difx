/************************************************************************/
/*                                                                      */
/*  Fills in a type_205 record                                          */
/*                                                                      */
/*      Inputs:         
/*                                                                      */
/*      Output:         t205        Filled in type_205 record           */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "vex.h"

int
fill_205 (
struct scan_struct *root,
struct type_pass *pass,
struct type_param *param,
struct type_203 *t203,
struct type_205 *t205)
    {
    int i, j, ch, nch, int_time, sb, ind,
        nchan, nfreqs;
    struct freq_corel *fc;
    nchan = (strncmp (t203->version_no, "00", 2) == 0) ? 32 : 8*MAXFREQ;

    clear_205 (t205);
                                        /* For now, UCT central is same as FRT */
    t205->utc_central.year = root->start_time.year;
    t205->utc_central.second = fmod ((double)param->reftime,  60.0);
    int_time = param->reftime;       /* In seconds */
    int_time /= 60;                  /* Now in minutes */
    t205->utc_central.minute = int_time % 60;
    int_time /= 60;                  /* Now in hours */
    t205->utc_central.hour = int_time % 24;
    t205->utc_central.day = int_time / 24 + 1; /* doy starts at 001 */
    t205->offset = 0.0;
                                        /* Skip fourfit execution modes for now */

                                        /* Search windows */
    t205->search[0] = param->win_sb[0];
    t205->search[1] = param->win_sb[1];
    t205->search[2] = param->win_dr[0];
    t205->search[3] = param->win_dr[1];
    t205->search[4] = param->win_mb[0];
    t205->search[5] = param->win_mb[1];
                                        /* Filtering thresholds NYI */
    
                                        /* Start and stop times for this pass */
    t205->start.year = root->start_time.year;
    t205->start.second = fmod ((double)pass->start,  60.0);
    int_time = pass->start;
    int_time /= 60;                  /* Now in minutes */
    t205->start.minute = int_time % 60;
    int_time /= 60;                  /* Now in hours */
    t205->start.hour = int_time % 24;
    t205->start.day = int_time / 24 + 1;

    t205->stop.year = root->start_time.year;
    t205->stop.second = fmod ((double)pass->stop,  60.0);
    int_time = pass->stop;
    int_time /= 60;                  /* Now in minutes */
    t205->stop.minute = int_time % 60;
    int_time /= 60;                  /* Now in hours */
    t205->stop.hour = int_time % 24;
    t205->stop.day = int_time / 24 + 1;

    t205->ref_freq = param->ref_freq;
    
    nfreqs = 0;
    for (ch=0; ch<MAXFREQ; ch++)
        {
        fc = pass->pass_data + ch;
        if (fc->frequency == 0.0 || nfreqs >= pass->nfreq) 
            continue;
        nfreqs++;
        t205->ffit_chan[ch].ffit_chan_id = fc->freq_code;
        nch = 0;
        for (sb=0; sb<2; sb++)
            {
            ind = sb + 2 * pass->pol;
            if (fc->index[ind] <= 0) 
                continue;
            for (j=0; j<nchan; j++)
                if (fc->index[ind] == t203->channels[j].index) 
                    break;
            if (j == nchan)
                {
                msg ("Could not find index number %d in type 203 record", 
                                                2, fc->index[ind]);
                return (-1);
                }
            if (nch >= 4)
                {
                msg ("Error - more than 4 correlator indices in ffit chan '%c'", 
                                                2, fc->freq_code);
                return (-1);
                }
            t205->ffit_chan[ch].channels[nch] = j;
            nch++;
            }
        }

    return (0);
    }
