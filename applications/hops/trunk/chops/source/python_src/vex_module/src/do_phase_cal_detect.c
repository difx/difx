/************************************************************************/
/*                                                                      */
/* Given a phase_cal_detect def and a station parameter structure, this */
/* routine extracts and stores all the relevant information             */
/*                                                                      */
/*      Inputs:         dl              pointer to def_list struct      */
/*                      nchan           Number of channels present      */
/*                                                                      */
/*      Output:         stn             The station struct to be filled */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created January 12, 1998 by CJL                                      */
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
do_phase_cal_detect (struct def_list *dl,
                     int nchan,
                     struct station_struct *stn)
    {
    int i, j, st, found;
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
        if (parse_pval (str, "PHASE_CAL_DETECT", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }

        if ISNAME ("phase_cal_detect")
            {
            link = p_val.dval[0].data.linkval;
            found = FALSE;
            for (i=0; i<nchan; i++)
                {
                ch = stn->channels + i;
                if (strcmp (link, ch->pcal_id) != 0) continue;
                if (p_val.nval > 17)
                    {
                    msg ("Too many phase_cal_detect numbers", 2);
                    return (-1);
                    }
                for (j=1; j<p_val.nval; j++)
                    ch->pcal_detect[j-1] = p_val.dval[j].data.intval;
                found = TRUE;
                }
            if (! found)
                {
                msg ("PHASE_CAL_DETECT id '%s' not found in $FREQ", 2, link);
                return (-1);
                }
            }
        }

    return (0);
    }
