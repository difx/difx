/************************************************************************/
/*                                                                      */
/* Given memory structures containing the type-3 file information for   */
/* both stations, and the corel time/frequency array, this routine      */
/* matches up the channels and APs with the state count entries in the  */
/* type-3 files, and inserts interpolated state count numbers into each */
/* cell of the time/frequency array.                                    */
/*                                                                      */
/*      Inputs:         sd1, sd2        sdata pointers for the 2 stns   */
/*                      param           Needed for start time etc.      */
/*                                                                      */
/*      Output:         corel           statecount numbers filled in    */
/*                                                                      */
/* Created 23 October 2001 by CJL, derived from pcal_interp()           */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "math.h"
#include "mk4_sizes.h"
                                        /* criterion for changes in state
                                         * count ratios implying a slipped
                                         * sync in the SU rjc 2002.7.18 */
#define EPSILON 0.09

enum {USB_LCP, LSB_LCP, USB_RCP, LSB_RCP, NONE};
enum {LCP, RCP};

void sort_time (double *, double *, double *, double *, double *, int);


int
stcount_interp (
struct mk4_sdata *sd1,
struct mk4_sdata *sd2,
struct type_param *param,
struct freq_corel *corel,
struct type_status *status)
    {
    int stn, j, sc, ap, ch, f, i, n;
    double start, stop, time[MAXSTATPER];
    double bigpos[MAXSTATPER], pos[MAXSTATPER], neg[MAXSTATPER], bigneg[MAXSTATPER];
    double bigposval, posval, negval, bignegval, totcount;
    double ratio, old_ratio;
    double lo_default, hi_default;
    char chan[9];
    char chan_buffer[MAXFREQ][9];
    int sc_index[MAXFREQ];
    int numchans, numscans, new_chan, is_unique;
    int ret, first_mess, npts, nstart;
    struct freq_corel *fc;
    struct mk4_sdata *sd;
    struct type_306 *t306;
    struct interp_sdata *isd;
                                        /* flag to control stc err message */
    first_mess = 1;
    status->stc_present = 0;         // indicate neither station having state counts
    
    if (param->bits_sample[0] == 2) // in mk4 xf mode, both stns have same bits/samp
        {
        lo_default = 0.32;
        hi_default = 0.18;
        }
    else                            // if 1 bit/sample, all samples are high
        {
        lo_default = 0.00;
        hi_default = 0.50;
        }

                                        /* Initialize stcount data */
    for (f=0; f<MAXFREQ; f++)
        {
        fc = corel + f;
        if (fc->frequency <= 0.0) continue;

        for (ap=0; ap<param->num_ap; ap++)
            {
            for (ch=0; ch<4; ch++)
                {                   // initialize fractions to nominal values
                isd = &(fc->data[ap].ref_sdata);
                isd->bigpos[ch] = hi_default;
                isd->pos[ch]    = lo_default;
                isd->neg[ch]    = lo_default;
                isd->bigneg[ch] = hi_default;
    
                isd = &(fc->data[ap].rem_sdata);
                isd->bigpos[ch] = hi_default;
                isd->pos[ch]    = lo_default;
                isd->neg[ch]    = lo_default;
                isd->bigneg[ch] = hi_default;
                }
            }
        }
                                        /* Do it one station at a time */
    for (stn=0; stn<2; stn++)
        {
        numchans = numscans = 0;
        sd = (stn == 0) ? sd1 : sd2;
                                        /* No stcount records - skip */
        if (sd->n306 == 0) 
            {
            msg ("No stcount data for station %c", 1, sd->t300->id);
            continue;
            }
                                    // indicate that this station has state counts
        status->stc_present |= 1 << stn;
                                        /* Loop over channels */
                                        /* Assume that first 306 is like */
                                        /* all the rest, vis a vis chan order */
        for (n=0; n<sd->n306; n++)
            {
            is_unique = 0;
            t306 = sd->t306[n];
            for (j=0; j<16; j++)
                {
                strncpy (chan, t306->stcount[j].chan_id, 8);
                if (strlen (chan) < 2)
                    continue;
                else
                    {
                    new_chan = 1;
                    for (i=0; i<numchans; i++)
                        {
                        if (!strncmp (chan, chan_buffer[i], 8))
                            {
                            new_chan = 0;
                            break;
                            }
                        }
                    if (new_chan)
                        {
                        msg ("new chan %s", 0, chan);
                        if (!is_unique)
                            {
                            msg ("is unique", 0);
                            is_unique = 1;
                            numscans++;
                            }
                        sc_index[numchans] = j;
                        strncpy (chan_buffer[numchans++], chan, 8);
                        }
                    }
                }
            }
            
        msg ("exited stcount loop, numscans=%d, numchans = %d\n", 0, numscans, numchans);

                                        /* Sanitize type 306 records before
                                         * processing them */
        for (j=0; j<sd->n306; j++)
            {
            t306 = sd->t306[j];
            if (t306->duration < 5. || t306->duration > 15.)
                {                       /* bogus duration, edit record out */
                msg ("Discarding state count for stn %d record %d due to duration %g",
                     0, stn, j, t306->duration);
                if (sd->n306-- == j)
                    break;              /* don't shift ptrs if last record */
                
                for (i=j; i<sd->n306; i++)
                    sd->t306[i] = sd->t306[i+1];
                j--;                    /* deleted jth record; must process (new) jth*/
                }
            }



        for (sc=0; sc<numchans; sc++)
            {
            strncpy (chan, chan_buffer[sc], 8);
                                        /* Find this channel in corel */
            ch = NONE;
            for (f=0; f<MAXFREQ; f++)
                {
                fc = corel + f;
                if (fc->frequency <= 0.0) continue;
                                        /* ch will have values 0-3 */
                if (strncmp (chan, fc->ch_usb_lcp[stn], 8) == 0)
                    ch = USB_LCP;
                else if (strncmp (chan, fc->ch_usb_rcp[stn], 8) == 0)
                    ch = USB_RCP;
                else if (strncmp (chan, fc->ch_lsb_lcp[stn], 8) == 0)
                    ch = LSB_LCP;
                else if (strncmp (chan, fc->ch_lsb_rcp[stn], 8) == 0)
                    ch = LSB_RCP;
                                        /* Found it? */
                if (ch != NONE) break;
                }
            if (ch == NONE)
                {
                msg ("Could not find channel '%.8s' in corel array", 1, chan);
                continue;
                }

                                        /* OK, proceed with this one */
                                        /* Fill value arrays with counts */
            msg ("processing %d 306 records", 0, sd->n306);
            for (j=0, n=0; j<sd->n306; j++, n++)
                {
                t306 = sd->t306[j];
                if (strncmp(chan, t306->stcount[sc_index[sc]].chan_id, 8))
                    {
                    n--;
                    continue;
                    }
                                        // channel matches - copy data into arrays

                                        /* Get mean time of each type-306 record */
                                        /* using same time base as data (sBOY) */
                time[n] = 86400.0 * (t306->time.day - 1)
                         + 3600.0 * t306->time.hour
                         +   60.0 * t306->time.minute
                         +          t306->time.second
                         +    0.5 * t306->duration;

                bigpos[n] = t306->stcount[sc_index[sc]].bigpos;
                pos[n] = t306->stcount[sc_index[sc]].pos;
                neg[n] = t306->stcount[sc_index[sc]].neg;
                bigneg[n] = t306->stcount[sc_index[sc]].bigneg;
                totcount = bigpos[n] + pos[n] + neg[n] + bigneg[n];
            
                                        /* try to detect byte slips by their
                                         * effect on state statistics 2002.7.18 rjc */
                ratio = (pos[n]) ? bigpos[n] / pos[n] : 999.0;
                if (n != 0 && first_mess && (fabs ((ratio - old_ratio) / old_ratio) > EPSILON))
                    {
                    msg ("potential byteslip for stn %c chan %s record %d ratio %lf, ref %lf",
                          1, param->baseline[stn], chan, n, ratio, old_ratio);
                    first_mess = 0;     /* only one message per fringe fit */
                    }
                old_ratio = ratio;
                
                                        /* Trap 0 divide, convert to fractions */
                if (totcount == 0.0) 
                    totcount = 1.0;
                bigpos[n] /= totcount;
                pos[n] /= totcount;
                neg[n] /= totcount;
                bigneg[n] /= totcount;
                }
            npts = n;                   // total number of matching output points

                                        // sort time array, adjust pos etc. accordingly
            
            sort_time (time, bigpos, pos, neg, bigneg, npts);
            
            
                                        /* Loop over aps, interpolating positive values */
            nstart = 0;
            for (ap=0; ap<param->num_ap; ap++)
                {
                                        /* Start and stop of AP */
                start = param->start + (ap * param->acc_period);
                stop = start + param->acc_period;
                                        /* Hopefully robust interpolation */
                ret = ap_mean (start, stop, time, bigpos, pos, npts, 
                               &nstart, &bigposval, &posval);
                if (ret > 0) 
                    continue;
                else if (ret < 0)
                    {
                    msg ("Interpolation error in stcount_interp()", 2);
                    return (-1);
                    }
                                        /* Apply these values to corel array */
                if (stn == 0)
                    isd = &(fc->data[ap].ref_sdata);
                else  
                    isd = &(fc->data[ap].rem_sdata);

                isd->bigpos[ch] = bigposval;
                isd->pos[ch] = posval;
                }   /* End + ap loop */

                                        /* Loop over aps, interpolating negative values */
            nstart = 0;
            for (ap=0; ap<param->num_ap; ap++)
                {
                                        /* Start and stop of AP */
                start = param->start + (ap * param->acc_period);
                stop = start + param->acc_period;
                                        /* Hopefully robust interpolation */
                ret = ap_mean (start, stop, time, bigneg, neg, npts, 
                               &nstart, &bignegval, &negval);
                if (ret > 0)
                    continue;
                else if (ret < 0)
                    {
                    msg ("Interpolation error in stcount_interp()", 2);
                    return (-1);
                    }
                                        /* Apply these values to corel array */
                if (stn == 0)
                    isd = &(fc->data[ap].ref_sdata);
                else  
                    isd = &(fc->data[ap].rem_sdata);

                isd->neg[ch] = negval;
                isd->bigneg[ch] = bignegval;
                }   /* End - ap loop */
            }       /* End channel loop */
        }       /* End loop over stations */

    return (0);
    }
    

void sort_time(double *time, double *bigpos, double *pos,
               double *neg, double *bigneg, int npts)
    {
    int i, swap = TRUE;
    void swap_doubles (double *, double *);
    
    while (swap)
        {
        swap = FALSE;
        
        for (i=0; i < npts-1; i++)
            {
            if (time[i] > time[i+1])
                {
                swap = TRUE;
                swap_doubles(&time[i], &time[i+1]);
                swap_doubles(&bigpos[i], &bigpos[i+1]);
                swap_doubles(&pos[i], &pos[i+1]);
                swap_doubles(&neg[i], &neg[i+1]);
                swap_doubles(&bigneg[i], &bigneg[i+1]);
                }
            } 
        }
    
    return;
    }
    

void swap_doubles(double *a, double *b)
    {
    double temp = *a;
    *b = *a;
    *a = temp;
    
    return;
    }
