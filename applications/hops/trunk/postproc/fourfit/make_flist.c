/************************************************************************/
/*                                                                      */
/* The job of this routine is to take the channel specifications of two */
/* stations, figure out which ones can be correlated against each other */
/* and which are simply different sidebands/polarizations of the same   */
/* channel, then fill a frequency-ordered table which maps directly     */
/* onto fourfit frequency id letters, and the frequency axis of the     */
/* main time vs frequency array.  Recorded in the fl array are the      */
/* chan_struct array indices for the four sideband/polarization         */
/* combinations for each station.                                       */
/*                                                                      */
/*      Inputs:         stn1, stn2      station structs from root       */
/*                                                                      */
/*      Output:         fl              List of parameters for channels */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created April 5 1998 by CJL                                          */
/*                                                                      */
/* changed BW_PCT to 50% to line up large offset f's      rjc 2004.10.5 */
/* parameterized BW_PCT to be param.fmatch_bw_pct         rjc 2013.3.7  */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "vex.h"
#include "freqlist.h"
#include "mk4_sizes.h"
#include "param_struct.h"

#define FALSE 0
#define TRUE 1

int
make_flist (/*stn1, stn2, fl)*/
struct station_struct *stn1,
struct station_struct *stn2,
struct freqlist *fl)
    {
    int i, j, nf, ch1, ch2, ch, minindex, index, mismatch, lsb, usb;
    double freqs[MAX_CHAN_PP], minfreq, freq, bw, fdiff;
    struct chan_struct *chnl, *st1ch, *st2ch;
    extern struct type_param param;

                                        /* Initialize fl array */
    for (i=0; i<MAX_CHAN_PP; i++)
        {
        fl[i].sky_frequency = -1.0;
        for (j=0; j<4; j++)
            fl[i].ref_chnos[j] = fl[i].rem_chnos[j] = -1;
        }
                                        /* Make a list of all freqs */
                                        /* which are distinct, and */
                                        /* present at both stations */
    nf = 0;
    for (ch1=0; ch1<MAX_CHAN_PP; ch1++)
        {
        st1ch = stn1->channels + ch1;
        freq = st1ch->sky_frequency;
        bw = st1ch->bandwidth;
        if (freq <= 0.0) continue;
        for (i=0; i<nf; i++)
            if (fabs (freq - freqs[i]) / bw < param.fmatch_bw_pct/100.0)
                break;
        if (i == nf)
            {
            for (ch2=0; ch2<MAX_CHAN_PP; ch2++)
                {
                st2ch = stn2->channels + ch2;
                                        /* Criterion is freq diff as fraction of bandwidth */
                fdiff = fabs (freq - st2ch->sky_frequency);
                if (fdiff/bw < param.fmatch_bw_pct/100.0)
                    break;
                }
            if (ch2 < MAX_CHAN_PP)
                {
                freqs[nf] = freq;
                nf++;
                }
            }
        }
                                        /* Switch to MHz */
    for (i=0; i<nf; i++) freqs[i] /= 1.0e6;
                                        /* Assign fl entries */
                                        /* by ascending frequency */
    for (i=0; i<nf; i++)
        {
        minfreq = 1.0e30;
        for (j=0; j<nf; j++)
            {
            if (freqs[j] < 0.0) continue;
            if (freqs[j] < minfreq) 
                {
                minfreq = freqs[j];
                minindex = j;
                }
            }
        fl[i].sky_frequency = minfreq;
        freqs[minindex] = -1.0;
        }
                                        /* Now fill in chan numbers */
                                        /* which correspond to */
                                        /* sidebands and polarizations */
                                        /* There should be 0 or 1 of each */
    for (i=0; i<nf; i++)
        {
        for (ch=0; ch<MAX_CHAN_PP; ch++)
            {
                                        /* Locate frequency entry */
            chnl = stn1->channels + ch;
            freq = chnl->sky_frequency / 1e6;
            bw = chnl->bandwidth / 1e6;
            if (fabs ((freq - fl[i].sky_frequency) / bw) > param.fmatch_bw_pct/100.0)
                continue;
                                        /* Which combo is it? */
                                        /* LL=0,UL=1,LR=2,UR=3 */
            index = 0;
            if (chnl->net_sideband == 'L') index += 1;
            if (chnl->polarization == 'R') index += 2;
                                        /* Should be one and only */
            if (fl[i].ref_chnos[index] >= 0)
                {
                msg ("Ambiguous ref channels in make_flist() freq[%d] %g index %d",
                     2, i, freq, index);
                return (-1);
                }
                                        /* Record the channel number */
            fl[i].ref_chnos[index] = ch;
            }
                                        /* Repeat for remote station */
        for (ch=0; ch<MAX_CHAN_PP; ch++)
            {
            chnl = stn2->channels + ch;
            freq = chnl->sky_frequency / 1e6;
            bw = chnl->bandwidth / 1e6;
                                        /* Remote station can be different freq, within limits */
            if (fabs ((freq - fl[i].sky_frequency) / bw) > param.fmatch_bw_pct/100.0)
                continue;
                                        /* Which combo is it? */
                                        /* LL=0,UL=1,LR=2,UR=3 */
            index = 0;
            if (chnl->net_sideband == 'L') index += 1;
            if (chnl->polarization == 'R') index += 2;
                                        /* Should be one and only */
            if (fl[i].rem_chnos[index] >= 0)
                {
                msg ("Ambiguous rem channels in make_flist() freq[%d] %g index %d",
                     2, i, freq, index);
                return (-1);
                }
                                        /* Record the channel number */
            fl[i].rem_chnos[index] = ch;
            }
        }
                                        /* Check that sideband pairs */
                                        /* come from the same physical BBC */
    for (i=0; i<nf; i++)
        {
        mismatch = TRUE;
                                        /* chan_struct LSB/USB indices */
        lsb = fl[i].ref_chnos[0];
        usb = fl[i].ref_chnos[1];
                                        /* If both present, are BBCs same? */
        if ((lsb >= 0) && (usb >= 0))
            if (stn1->channels[lsb].bbc_no != stn1->channels[usb].bbc_no) break;
                                        /* Repeat for all combinations */
        lsb = fl[i].ref_chnos[2];
        usb = fl[i].ref_chnos[3];
        if ((lsb >= 0) && (usb >= 0))
            if (stn1->channels[lsb].bbc_no != stn1->channels[usb].bbc_no) break;

        lsb = fl[i].rem_chnos[0];
        usb = fl[i].rem_chnos[1];
        if ((lsb >= 0) && (usb >= 0))
            if (stn2->channels[lsb].bbc_no != stn2->channels[usb].bbc_no) break;

        lsb = fl[i].rem_chnos[2];
        usb = fl[i].rem_chnos[3];
        if ((lsb >= 0) && (usb >= 0))
            if (stn2->channels[lsb].bbc_no != stn2->channels[usb].bbc_no) break;

        mismatch = FALSE;
        }
    if (mismatch)
        {
        msg ("Mismatched USB/LSB pair in make_flist()", 2);
        return (-1);
        }

    return (0);
    }
