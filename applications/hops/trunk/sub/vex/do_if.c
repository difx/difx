/************************************************************************/
/*                                                                      */
/* Given an if def and a station parameter structure, this routine      */
/* extracts and stores all the relevant information                     */
/*                                                                      */
/*      Inputs:         dl              pointer to de_list struct       */
/*                      nchan           Number of st channels present   */
/*                                                                      */
/*      Output:         stn             The station struct to be filled */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created January 9, 1998 by CJL                                       */
/* Rewritten for Haystack parser, 30 October 1998 by CJL                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
do_if (struct def_list *dl,
       int nchan,
       struct station_struct *stn)
    {
    int i, st;
    char *str, *link;
    struct def *thisdef;
    struct chan_struct *ch;
    struct param_val p_val;
    extern struct block blist[];
    extern struct statement *stlist;
    extern int ovex_ver;
                                        /* Def had better be there */
    thisdef = blist[dl->blockno].deflist + dl->defno;
    if (thisdef == NULL) return (1);
                                        /* Extract low level statements */
                                        /* in the def */
    for (st=thisdef->start+1; st<thisdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "IF", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }

        if ISNAME ("if_def")
            {
            link = p_val.dval[0].data.linkval;
            for (i=0; i<nchan; i++)
                {
                                        /* Find referenced channel(s) */
                ch = stn->channels + i;
                if (strcmp (link, ch->if_id) != 0) continue;
                                        /* Get values */
                ch->polarization = p_val.dval[2].data.strval[0];
                ch->if_total_lo = p_val.dval[3].data.realval;
                ch->if_sideband = p_val.dval[4].data.strval[0];
                if (p_val.nval > 5)
                    {
                    ch->pcal_spacing = p_val.dval[5].data.realval;
                    if (p_val.nval == 7)
                        ch->pcal_base_freq = p_val.dval[6].data.realval;
                    else ch->pcal_base_freq = 0.0;
                    }
                }
            }
        }

    return (0);
    }
