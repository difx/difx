/************************************************************************/
/*                                                                      */
/* New routine for Mk4, which takes the place of fill_freq_table() and  */
/* set_pointers() in the Mk3 version.  It figures out how to set up and */
/* label the fourfit channels in the freq_corel channel array "corel"   */
/* and then proceeds to set the corel record pointers in the            */
/* appropriate cells for sideband, polarization combination, channel,   */
/* and ap number relative to the start of good data.  The resulting     */
/* time/frequency array is then ready for filtering and fitting         */
/* operations.                                                          */
/*                                                                      */
/*      Inputs:         root            Needed for freq information     */
/*                      cdata           memory copy of corel file info  */
/*                      param           Some useful numbers from        */
/*                                      earlier steps                   */
/*                                                                      */
/*      Output:         corel           Main array, filled in.          */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created April 6 1998 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mk4_data.h"
#include "vex.h"
#include "pass_struct.h"
#include "param_struct.h"
#include "freqlist.h"
#include "mk4_sizes.h"

int 
set_pointers (/*stn1, stn2, cdata, param, corel)*/
struct station_struct *stn1,
struct station_struct *stn2,
struct mk4_corel *cdata,
struct type_param *param,
struct freq_corel *corel)
    {
    int i, j, fqno, st1ch, st2ch, ref, rem, ch, ap, sb, first;
    int sideband, polarization, tindex;
    struct chan_struct *refch, *remch;
    struct freq_corel *fc;
    struct type_101 *t101;
    char st1, st2;
    struct index_tag *idx;
    struct freqlist flist[MAXFREQ];
    
                                        /* Some initialization */
    for (i=0; i<MAXFREQ; i++) clear_freq_corel (corel+i);
                                        /* Make a list of frequencies in the */
                                        /* root, which translates to fourfit */
                                        /* frequency channel letter ids */
    if (make_flist (stn1, stn2, flist) != 0)
        {
        msg ("Error figuring out list of fourfit frequencies", 2);
        return (-1);
        }
                                        /* Loop over all indices */
    first = TRUE;
    for (i=0; i<cdata->index_space; i++)
        {
        idx = cdata->index + i;
        if ((t101 = idx->t101) == NULL) continue;
                                        /* Get chan array numbers from ids */
        st1ch = st2ch = -1;             /* first, initialize to catch non-detections */
        for (ch=0; ch<MAX_CHAN; ch++)
            {
            refch = stn1->channels + ch;
            remch = stn2->channels + ch;
            if (strcmp (refch->chan_name, t101->ref_chan_id) == 0) st1ch = ch;
            if (strcmp (remch->chan_name, t101->rem_chan_id) == 0) st2ch = ch;
            }
        
        if (st1ch < 0)                  // sanity check
            {
            msg ("Can't identify ref stn channel in t101: %s does not appear in root", 
                 2, t101->ref_chan_id);
            return (-1);
            }
        if (st2ch < 0)                  // sanity check, part deux
            {
            msg ("Can't identify rem stn channel in t101: %s does not appear in root", 
                 2, t101->rem_chan_id);
            return (-1);
            }
        refch = stn1->channels + st1ch;
        remch = stn2->channels + st2ch;
                                        /* Is it a valid baseline pair? */
        for (fqno=0; fqno<MAXFREQ; fqno++)
            {
            if (flist[fqno].sky_frequency < 0.0) continue;
                                        /* Loop over sideband/pol combos as */
                                        /* set up by make_flist */
            for (ref=0; ref<4; ref++) 
                if (st1ch == flist[fqno].ref_chnos[ref]) break;
            for (rem=0; rem<4; rem++) 
                if (st2ch == flist[fqno].rem_chnos[rem]) break;
            if ((ref < 4) && (rem < 4)) break;
            }
                                        /* Not found, error */
        if (fqno == MAXFREQ)
            {
            msg ("Invalid channel ids in type 101 record, '%s' '%s'", 2,
                        t101->ref_chan_id, t101->rem_chan_id);
            return (-1);
            }
                                        /* Check that sidebands match. */
                                        /* 0, 2 are usb, 1, 3 are lsb */
        if ((ref % 2) != (rem % 2))
            {
            msg ("Mismatched sidebands for index number %d", 2, t101->index);
            return (-1);
            }
                                        /* Get sideband (0=USB,1=LSB) and pol'n */
        sideband = ref % 2;
        if (ref == rem)
            {
            if (ref < 2) polarization = POL_LL;
            else polarization = POL_RR;
            }
        else
            {
            if (ref < rem) polarization = POL_LR;
            else polarization = POL_RL;
            }

                                        /* Now start filling up corel struct */
        fc = corel + fqno;
        fc->freq_code = fchars[fqno];
        fc->frequency = flist[fqno].sky_frequency;
                                        /* Frequency group is 1st char of */
                                        /* channel id by Mk4 corr. convention */
        if (fc->fgroup == ' ') fc->fgroup = t101->ref_chan_id[0];
        if ((fc->fgroup != t101->ref_chan_id[0])
                || (fc->fgroup != t101->rem_chan_id[0]))
            {
            msg ("Mismatching frequency groups in set_pointers", 2);
            return (-1);
            }
        fc->index[sideband + 2*polarization] = t101->index;
                                        /* BBCs and tracks */
        if ((polarization == POL_LL) || (polarization == POL_LR))
            {
            fc->bbc_lcp[0] = refch->bbc_no;
            for (j=0; j<4; j++)
                {
                //printf ("LLLR ref j %d sign %d mag %d\n", j, refch->sign_tracks[j], refch->mag_tracks[j]);
                if (refch->sign_tracks[j] != I_UNDEFINED)
                    fc->trk_lcp[0][j+8*sideband] = refch->sign_tracks[j];
                if (refch->mag_tracks[j] != I_UNDEFINED)
                    fc->trk_lcp[0][j+4+8*sideband] = refch->mag_tracks[j];
                }
            }
        if ((polarization == POL_RL) || (polarization == POL_RR))
            {
            fc->bbc_rcp[0] = refch->bbc_no;
            for (j=0; j<4; j++)
                {
                //printf ("RLRR ref j %d sign %d mag %d\n", j, refch->sign_tracks[j], refch->mag_tracks[j]);
                if (refch->sign_tracks[j] != I_UNDEFINED)
                    fc->trk_rcp[0][j+8*sideband] = refch->sign_tracks[j];
                if (refch->mag_tracks[j] != I_UNDEFINED)
                    fc->trk_rcp[0][j+4+8*sideband] = refch->mag_tracks[j];
                }
            }
        if ((polarization == POL_LL) || (polarization == POL_RL))
            {
            fc->bbc_lcp[1] = remch->bbc_no;
            for (j=0; j<4; j++)
                {
                //printf ("LLRL rem j %d sign %d mag %d\n", j, remch->sign_tracks[j], remch->mag_tracks[j]);
                if (remch->sign_tracks[j] != I_UNDEFINED)
                    fc->trk_lcp[1][j+8*sideband] = remch->sign_tracks[j];
                if (remch->mag_tracks[j] != I_UNDEFINED)
                    fc->trk_lcp[1][j+4+8*sideband] = remch->mag_tracks[j];
                }
            }
        if ((polarization == POL_LR) || (polarization == POL_RR))
            {
            fc->bbc_rcp[1] = remch->bbc_no;
            for (j=0; j<4; j++)
                {
                //printf ("LRRR rem j %d sign %d mag %d\n", j, remch->sign_tracks[j], remch->mag_tracks[j]);
                if (remch->sign_tracks[j] != I_UNDEFINED)
                    fc->trk_rcp[1][j+8*sideband] = remch->sign_tracks[j];
                if (remch->mag_tracks[j] != I_UNDEFINED)
                    fc->trk_rcp[1][j+4+8*sideband] = remch->mag_tracks[j];
                }
            }
                                        /* Record the channel ids */
        if (sideband == 0)
            {
            if ((polarization == POL_LL) || (polarization == POL_LR))
                strncpy (fc->ch_usb_lcp[0], t101->ref_chan_id, 8);
            if ((polarization == POL_RL) || (polarization == POL_RR))
                strncpy (fc->ch_usb_rcp[0], t101->ref_chan_id, 8);
            if ((polarization == POL_LL) || (polarization == POL_RL))
                strncpy (fc->ch_usb_lcp[1], t101->rem_chan_id, 8);
            if ((polarization == POL_LR) || (polarization == POL_RR))
                strncpy (fc->ch_usb_rcp[1], t101->rem_chan_id, 8);
            }
        else if (sideband == 1)
            {
            if ((polarization == POL_LL) || (polarization == POL_LR))
                strncpy (fc->ch_lsb_lcp[0], t101->ref_chan_id, 8);
            if ((polarization == POL_RL) || (polarization == POL_RR))
                strncpy (fc->ch_lsb_rcp[0], t101->ref_chan_id, 8);
            if ((polarization == POL_LL) || (polarization == POL_RL))
                strncpy (fc->ch_lsb_lcp[1], t101->rem_chan_id, 8);
            if ((polarization == POL_LR) || (polarization == POL_RR))
                strncpy (fc->ch_lsb_rcp[1], t101->rem_chan_id, 8);
            }
                                        /* Allocate data array memory, if not */
                                        /* already done for another index no. */
        if (! fc->data_alloc)
            {
            fc->data = (struct data_corel *)
                                calloc (param->maxap + 1, sizeof(struct data_corel));
            if (fc->data == NULL)
                {
                perror("Calloc failure");
                msg ("Memory allocation failure for data pointer array");
                fc->data_alloc = FALSE;
                return(-1);
                }
            else fc->data_alloc = TRUE;
                                        /* Initialize type-120 ptrs to null */
            for (ap=0; ap<param->maxap + 1; ap++)
                for (sb=0; sb<2; sb++)
                    {
                    fc->data[ap].apdata_ll[sb] = NULL;
                    fc->data[ap].apdata_rr[sb] = NULL;
                    fc->data[ap].apdata_lr[sb] = NULL;
                    fc->data[ap].apdata_rl[sb] = NULL;
                    }
            }
                                        /* Now ready to set pointers in the */
                                        /* main time/freq array */
        
        for (ap=0; ap<idx->ap_space; ap++)
            {
            if (idx->t120[ap] == NULL) continue;
                                        /* Convenient place to record the */
                                        /* correlation mode */
                                        /* Due to method of storage, must */
                                        /* replicate lags, hence nlags *= 2 */
            if (first) 
                {
                param->cormode = idx->t120[ap]->type;
                if ((param->cormode == AUTO_GLOBAL) || (param->cormode == AUTO_PER_LAG))
                    param->nlags *= 2;
                }
            first = FALSE;

            tindex = idx->t120[ap]->ap - param->minap;
                                        // negative tindex indicates data before scheduled
                                        // scan start, so discard it
            if (tindex < 0)
                continue;
                                        /* set flags to indicate data presence */
            fc->data[tindex].flag |= 1 << (2 * polarization + sideband);
            switch (polarization)
                {
                case POL_LL:
                    fc->data[tindex].apdata_ll[sideband] = idx->t120[ap];
                    break;
                case POL_RR:
                    fc->data[tindex].apdata_rr[sideband] = idx->t120[ap];
                    break;
                case POL_LR:
                    fc->data[tindex].apdata_lr[sideband] = idx->t120[ap];
                    break;
                case POL_RL:
                    fc->data[tindex].apdata_rl[sideband] = idx->t120[ap];
                }
            }                           /* End ap loop */
        }                               /* End index loop */
    return(0);
    }
