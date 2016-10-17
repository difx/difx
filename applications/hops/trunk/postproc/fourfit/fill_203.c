/************************************************************************/
/*                                                                      */
/*  Fills in a type_203 record, based on root and param information     */
/*                                                                      */
/*      Inputs:         root        scan_info struct                    */
/*                      param       lots of parameters                  */
/*                                                                      */
/*      Output:         t203        Filled in type_203 record           */
/*                                                                      */
/* Created 31 August 1999 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <time.h>
#include <string.h>
#include "vex.h"
#include "mk4_data.h"
#include "param_struct.h"
#include "mk4_sizes.h"

int
fill_203 (/* root, param, t203) */
struct scan_struct *root,
struct type_param *param,
struct type_203 *t203)
    {
    extern struct mk4_corel cdata; 
    struct station_struct *refst, *remst;
    struct chan_struct *refch, *remch;
    int i, ch, chfound, rootch;
    struct type_101 *t101;
    clear_203 (t203);
                                        /* Get stations from root */
    refst = remst = NULL;
    for (i=0; i<root->nst; i++)
        {
        if (root->st[i].mk4_site_id == param->baseline[0]) refst = root->st + i;
        if (root->st[i].mk4_site_id == param->baseline[1]) remst = root->st + i;
        }
    if ((refst == NULL) || (remst == NULL))
        {
        if (refst == NULL)
            msg ("Failed to find station '%c' in ovex file", 
                                                2, param->baseline[0]);
        else
            msg ("Failed to find station '%c' in ovex file", 
                                                2, param->baseline[1]);
        return (-1);
        }


    ch = 0;
    for (i=0; i<cdata.index_space; i++)
        {
        t101 = cdata.index[i].t101;
                                        /* Empty slot? */
        if (t101 == NULL) continue;
                                        /* Is this a mirror? If so, skip */
/*         if (i != t101->primary) continue; */

        t203->channels[ch].index = t101->index;
                                        /* Assume this is constant for all channels */
                                        /* and both stations */
        t203->channels[ch].sample_rate = 1.0 / param->samp_period;
        strncpy (t203->channels[ch].ref_chan_id, t101->ref_chan_id, 8);
        strncpy (t203->channels[ch].rem_chan_id, t101->rem_chan_id, 8);
                                        /* Find the channel name in root */
        chfound = 0;
        for (rootch=0; rootch<MAX_CHAN; rootch++)
            {
            if (strncmp (t203->channels[ch].ref_chan_id, 
                        refst->channels[rootch].chan_name, 8) == 0)
                {
                t203->channels[ch].refsb = refst->channels[rootch].net_sideband;
                t203->channels[ch].refpol = refst->channels[rootch].polarization;
                t203->channels[ch].ref_freq = refst->channels[rootch].sky_frequency;
                chfound++;
                }
            if (strncmp (t203->channels[ch].rem_chan_id, 
                        remst->channels[rootch].chan_name, 8) == 0)
                {
                t203->channels[ch].remsb = remst->channels[rootch].net_sideband;
                t203->channels[ch].rempol = remst->channels[rootch].polarization;
                t203->channels[ch].rem_freq = remst->channels[rootch].sky_frequency;
                chfound++;
                }
            }
        if (chfound != 2)
            {
            msg ("Could not find channel (%s,%s) in root", 2, t101->ref_chan_id,
                                    t101->rem_chan_id);
            return (-1);
            }
                                        /* Point to next t203 channel */
        ch++;
        if (ch == 8 * MAXFREQ)      // ensure there aren't too many channels
            {
            msg ("Too many (%d) t101 channels for t203 record", 2, ch);
            return -1;
            }
        }

    return (0);
    }
