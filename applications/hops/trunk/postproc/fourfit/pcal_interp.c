/************************************************************************/
/*                                                                      */
/* Given memory structures containing the type-3 file information for   */
/* both stations, and the corel time/frequency array, this routine      */
/* matches up the channels and APs with the phasecal entries in the     */
/* type-3 files, and inserts interpolated phasecal vectors into each    */
/* cell of the time/frequency array.                                    */
/*                                                                      */
/*      Inputs:         sd1, sd2        sdata pointers for the 2 stns   */
/*                      param           Needed for start time etc.      */
/*                                                                      */
/*      Output:         corel           pcal numbers filled in          */
/*                                                                      */
/* Created 13 April 1998 by CJL                                         */
/* Added support for type 309 records               rjc  2006.8.2       */
/* extended SU pcal by 30s at both ends             rjc  2010.5.24      */
/* modified for v.1 type 309's (64 ch & tones)      rjc  2010.10.1      */
/* flip pcal sign for LSB for corr date > 2012y124d rjc  2012.6.13      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "mk4_sizes.h"

#define TWO31 2147483648.0
#define TWO32 4294967296.0

enum {USB_LCP = 0, USB_RCP, LSB_LCP, LSB_RCP, NONE};
enum {LCP, RCP};

int pcal_interp (struct mk4_sdata *sd1,
                 struct mk4_sdata *sd2,
                 struct type_param *param,
                 struct freq_corel *corel)
    {
    int stn, j, pc, ap, ch, f, ipc, ipcmin, ipcmax, i, n,
        pc_index[MAXFREQ],
        numchans, new_chan, npts, nstart,
        ret, do309, numrecs, pc_max, jmax, tshift,
        iyr, idoy, idate, after_2012_124;

    double start, stop, time[MAXSTATPER], pc_real[MAXSTATPER], pc_imag[MAXSTATPER];
    double realval, imagval, freq, u, v;
    char chan[9];
    char chan_buffer[MAXFREQ][9];
    struct freq_corel *fc;
    struct mk4_sdata *sd;
    struct type_308 *t308;
    struct type_309 *t309;
    struct interp_sdata *isd;
    extern int do_accounting;
    extern struct mk4_corel cdata;
    
    for (i=0; i<MAXFREQ; i++)
        pc_index[i] = -1;
                                        /* Initialize pcal arrays and data */
    for (f=0; f<MAXFREQ; f++)
        {
        fc = corel + f;
        if (fc->frequency <= 0.0) continue;

        for (stn=0; stn<2; stn++)
            for (j=0;j<MAX_PCF; j++)
                fc->pc_freqs[stn][j] = 0;

        for (ap=0; ap<param->num_ap; ap++)
            {
            isd = &(fc->data[ap].ref_sdata);
            for (j=0; j<MAX_PCF; j++)
                {
                isd->phasecal_lcp[j] = 0.0;
                isd->phasecal_rcp[j] = 0.0;
                }
            isd->pcweight_lcp = 0.0;
            isd->pcweight_rcp = 0.0;

            isd = &(fc->data[ap].rem_sdata);
            for (j=0; j<MAX_PCF; j++)
                {
                isd->phasecal_lcp[j] = 0.0;
                isd->phasecal_rcp[j] = 0.0;
                }
            isd->pcweight_lcp = 0.0;
            isd->pcweight_rcp = 0.0;
            }
        }

                                        // test for correlation date after pcal sign flip
    sscanf (cdata.id->date, "%4d%3d", &iyr, &idoy);
    idate = 1000 * iyr + idoy;
    after_2012_124 = idate > 2012124;
                                        /* Do it one station at a time */
    for (stn=0; stn<2; stn++)
        {
        numchans = 0;
        sd = (stn == 0) ? sd1 : sd2;
                                        // check for type of phasecal records
        if (sd->n308 != 0) 
            do309 = FALSE;
        else if (sd->n309 != 0)
            do309 = TRUE;
        else                            // no pcal data - skip this station
            {
            msg ("No phasecal data for station %c", 1, sd->t300->id);
            continue;
            }

                                        /* Loop over type 308 or 309 records */
        numrecs = (do309) ? sd->n309 : sd->n308;
                                        /* Loop over channels */
                                        /* Assume that first 308/9 is like */
                                        /* all the rest, vis a vis chan order */
        pc_max = (do309) ? MAXFREQ : 32;
        for (n=0; n<numrecs; n++)
            {
            jmax = do309 ? MAXFREQ : 32;
            for (j=0; j<jmax; j++)
                {
                if (do309)          // copy in appropriate channel name
                    strncpy (chan, sd->t309[n]->chan[j].chan_name, 8);
                else
                    strncpy (chan, sd->t308[n]->pcal[j].chan_id, 8);
                
                if (strlen (chan) < 2)
                    continue;       // skip over null channel
                else
                    {
                    new_chan = TRUE;
                    for (i=0; i<numchans; i++)
                        if (!strncmp (chan, chan_buffer[i], 8))
                            {       // found this channel already in buffer
                            new_chan = FALSE;
                            break;
                            }
                        
                    if (new_chan)
                        {
                        pc_index[numchans] = j;
                        strncpy (chan_buffer[numchans], chan, 8);
                        msg ("pcal_interp chan_buffer[%d] %s", -1, 
                              numchans, chan_buffer[numchans]);
                        numchans++;
                        }
                    }
                }
            }
        msg ("exited pcal loop, numchans = %d\n", 0, numchans);

                                        // loop over channels for interpolating        
        for (pc=0; pc < (do309 ? numchans : pc_max); pc++)
            {
            if (do309)
                {
                strncpy (chan, chan_buffer[pc], 8);
                }
            else
                {
                t308 = sd->t308[0];
                strncpy (chan, t308->pcal[pc].chan_id, 8);
                }
            msg ("pc = %d, chan = %s", -1, pc, chan);

            if (strlen (chan) < 2)      // skip over null channels
                continue;
                                        /* Find this channel in corel */
            ch = NONE;
                                        // fc is link to appropriate data channel
                                        // ch will indicate polarization & sideband
            for (f=0; f<MAXFREQ; f++)
                {
                fc = corel + f;
                if (fc->frequency <= 0.0)
                    continue;

                if (strncmp (chan, fc->ch_usb_lcp[stn], 8) == 0)
                    ch = USB_LCP;
                else if (strncmp (chan, fc->ch_usb_rcp[stn], 8) == 0)
                    ch = USB_RCP;
                else if (strncmp (chan, fc->ch_lsb_lcp[stn], 8) == 0)
                    ch = LSB_LCP;
                else if (strncmp (chan, fc->ch_lsb_rcp[stn], 8) == 0)
                    ch = LSB_RCP;
                                        /* Found it? */
                if (ch != NONE) 
                    break;
                }
            if (ch == NONE)
                {
                msg ("Could not find channel '%.8s' in corel array", 1, chan);
                continue;
                }

                                        // matching record of type 309 is used to
                                        // find frequencies of tones for this chan
            if (do309)
                {
                for (i=0; i<numrecs; i++)
                    {
                    t309 = sd->t309[i];
                    if (strncmp(chan, t309->chan[pc_index[pc]].chan_name, 8) == 0)
                        {
                        for (j=0; j<MAX_PCF; j++)
                            fc->pc_freqs[stn][j]  = t309->chan[j].freq;
                        break;
                        }
                    }
                }
                
                                        // allow lsb pcal rjc 2007.2.1
            // if ((ch == LSB_LCP) || (ch == LSB_RCP)) continue;


                                        /* either find this tone in list, or
                                         * add it to the list, iff type 308 */
            if (do309)
                {
                ipcmin = 0;             // we will be copying all tones for t309
                ipcmax = MAXFREQ - 1;
                }
            else                        // figure out which single tone for t308
                {
                for (ipc=0; ipc<MAX_PCF; ipc++)
                    if (fc->pc_freqs[stn][ipc] == t308->pcal[pc].frequency)
                        break;
                    else if (fc->pc_freqs[stn][ipc] == 0.0)
                        {
                        fc->pc_freqs[stn][ipc] = t308->pcal[pc].frequency;
                        break;
                        }
                                        /* complain about table overrun */
                if (ipc == MAX_PCF)
                    {
                    msg ("skipping tone at frequency %g", 1, t308->pcal[pc].frequency);
                    continue;
                    }
                ipcmin = ipc;       // just do this one tone in following code
                ipcmax = ipc;
                }
                                    // OK, proceed with the tones to be copied
                                    // Fill value arrays with pcal with appropriate
                                    // record types
            for (ipc=ipcmin; ipc<=ipcmax; ipc++)
                {
                for (j=0, n=0; j<numrecs; j++, n++)
                    {
                    if (do309)
                        {
                        t309 = sd->t309[j];
                                    // skip record if wrong channel or tone not present
                        if (strncmp(chan, t309->chan[pc_index[pc]].chan_name, 8)
                            || ipc > t309->ntones)
                            {
                            n--;
                            continue;
                            }
                            
                        u = t309->chan[pc_index[pc]].acc[ipc][0];
                        v = t309->chan[pc_index[pc]].acc[ipc][1];
                                    // correct for 2's complement arithmetic
                        u = (u < TWO31) ? u : u - TWO32;
                        v = (v < TWO31) ? v : v - TWO32;
                                    // scale such that 1000 = 100% correlation
                                    // and match SU phase by shifting 180 degrees
                        pc_real[n] = u * param->samp_period / (-128.0 * t309->acc_period);
                        pc_imag[n] = v * param->samp_period / (-128.0 * t309->acc_period);
                        time[n] = t309->rot / 32e6 + 0.5 * t309->acc_period;
                        }
                    else
                        {
                        t308 = sd->t308[n];
                        pc_real[n] = t308->pcal[pc].real;
                        pc_imag[n] = t308->pcal[pc].imaginary;
                                        /* Get mean time of each type-308 record */
                                        /* using same time base as data (sBOY) */
                        time[n] = 86400.0 *(t308->time.day - 1)
                                 + 3600.0 * t308->time.hour
                                 +   60.0 * t308->time.minute
                                 +          t308->time.second
                                 +    0.5 * t308->duration;
                        }
                    }
                    npts = n;
                    if (npts == 0)
                        continue;       // no points to interpolate
                                        /* Loop over aps, interpolating values */
                nstart = 0;
		        if (do_accounting) 
                    account ("Organize data");
                for (ap=0; ap<param->num_ap; ap++)
                    {
                                        /* Start and stop of AP */
                    start = param->start + (ap * param->acc_period);
                    stop = start + param->acc_period;
                                        /* Hopefully robust interpolation */
                    ret = ap_mean (start, stop, time, pc_real, pc_imag, npts, 
                                  &nstart, &realval, &imagval);
                    if (ret > 0) 
                        continue;
                    else if (ret < 0)
                        {
                        msg ("Interpolation error in pcal_interp()", 2);
                        return (-1);
                        }
                                        // correct for LSB pcal sign flip after 2012y124d
                    if (after_2012_124
                     && param->corr_type == DIFX
                     && (ch == LSB_LCP || ch == LSB_RCP))
                        imagval = -imagval;
                                        /* Apply these values to corel array */
                    if (stn == 0)
                        isd = &(fc->data[ap].ref_sdata);
                    else  
                        isd = &(fc->data[ap].rem_sdata);

                    switch (ch)
                        {
                                        /* Need sign flip and 180-deg offset to */
                                        /* match Mk3 convention, done by flipping */
                                        /* sign of real part */
                        case USB_LCP:
                        case LSB_LCP:
                            if (do309)
                                {
                                    // if there is DSB data, then don't allow the LSB
                                    // pcal to overwrite what is already there
                                if (ch == LSB_LCP && cabs(isd->phasecal_lcp[ipc]) != 0.0)
                                    break;
                                    // must renormalize to account for fraction of high data
                                    // see rjc's normalization notes from 2006.10.16
                                if (param->corr_type == MK4HDW)
                                    {
                                    realval *= 128.0 / ((isd->bigpos[ch] + isd->bigneg[ch]) 
                                                                       * 84.66666 + 42.33333);
                                    imagval *= 128.0 / ((isd->bigpos[ch] + isd->bigneg[ch]) 
                                                                       * 84.66666 + 42.33333);
                                    }
                                }

                            isd->phasecal_lcp[ipc] = -realval + I * imagval;
                            if ((realval != 0.0) || (imagval != 0.0))
                                isd->pcweight_lcp = 1.0;
                            break;
                        case USB_RCP:
                        case LSB_RCP:
                            if (do309)
                                {
                                    // if there is DSB data, then don't allow the LSB
                                    // pcal to overwrite what is already there
                                if (ch == LSB_RCP && cabs (isd->phasecal_rcp[ipc]) != 0.0)
                                    break;
                                    // must renormalize to account for fraction of high data
                                    // see rjc's normalization notes from 2006.10.16
                                if (param->corr_type == MK4HDW)
                                    {
                                    realval *= 128.0 / ((isd->bigpos[ch] + isd->bigneg[ch]) 
                                                                       * 84.66666 + 42.33333);
                                    imagval *= 128.0 / ((isd->bigpos[ch] + isd->bigneg[ch]) 
                                                                       * 84.66666 + 42.33333);
                                    }
                                }

                            isd->phasecal_rcp[ipc] = -realval + I * imagval;
                            if ((realval != 0.0) || (imagval != 0.0))
                                isd->pcweight_rcp = 1.0;
                            break;
                        default:
                            break;
                        }
                    }               // End ap loop
		        if (do_accounting) 
                    account ("AP mean");
                }                   // end ipc loop
            }                       // End channel loop
        }                           // End loop over stations

    return (0);
    }
