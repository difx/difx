/************************************************************************/
/*                                                                      */
/* Given a freq def and a station parameter structure, this routine     */
/* extracts and stores all the relevant information, counting channels  */
/* as it goes.                                                          */
/*                                                                      */
/*      Inputs:         dl              pointer to de_list struct       */
/*                                                                      */
/*      Output:         stn             The station struct to be filled */
/*                      nchan           number of channels found        */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 9 January 1998 by CJL                                        */
/* Rewritten for Haystack parser, 30 October 1998 by CJL                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_sizes.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
do_freq (struct def_list *dl,
         struct station_struct *stn,
         int *nchan)
    {
    int st;
    char *str;

    struct def *thisdef;
    struct param_val p_val;
    struct chan_struct *ch;
    extern struct block blist[];
    extern struct statement *stlist;
    extern int ovex_ver;
                                        /* The def had better be there */
    thisdef = blist[dl->blockno].deflist + dl->defno;
    if (thisdef == NULL) return (1);
                                        /* Extract low level statements */
                                        /* in the def */
    for (st=thisdef->start+1; st<thisdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "FREQ", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }
        if ISNAME ("chan_def")
            {
                                        /* Point at current channels array element */
            if (*nchan >= MAX_CHAN)
                    {
                    msg ("Too many channels in $FREQ def", 2);
                    return (1);
                    }
            ch = stn->channels + *nchan;
                                        /* Fill in all the fields we can */
/*          ch->polarization =  */
            strcpy (ch->chan_name, p_val.dval[0].data.strval);
            strcpy (ch->band_id, p_val.dval[1].data.linkval);
            ch->sky_frequency = p_val.dval[2].data.realval;
            ch->net_sideband = p_val.dval[3].data.strval[0];
            ch->bandwidth = p_val.dval[4].data.realval;
            strcpy (ch->chan_id, p_val.dval[5].data.linkval);
            strcpy (ch->bbc_id, p_val.dval[6].data.linkval);
            strcpy (ch->pcal_id, p_val.dval[7].data.linkval);
                                        /* Increment channel pointer */
            (*nchan)++;
            }
        else if ISNAME ("sample_rate") 
            stn->samplerate = p_val.dval[0].data.realval;
        }
    return (0);
    }
