/************************************************************************/
/*                                                                      */
/* Given an pass_order def and a station parameter structure, this      */
/* routine extracts and stores all the relevant information             */
/*                                                                      */
/*      Inputs:         dl              pointer to def_list struct      */
/*                                                                      */
/*      Output:         stn             The station struct to be filled */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created January 9, 1998 by CJL                                       */
/* Rewritten for Haystack parser, 2 November 1998 by CJL                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
do_pass_order (struct def_list *dl,
               struct station_struct *stn)
    {
    int i, st;
    char *str, pass_string[10];
    struct def *thisdef;
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
        if (parse_pval (str, "PASS_ORDER", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }

        if ISNAME ("pass_order")
            {
            sprintf (pass_string, "%d%c", stn->passno, stn->subpass);
            for (i=0; i<p_val.nval; i++)
                {
                if (strcmp (pass_string, p_val.dval[i].data.strval) == 0)
                    break;
                }
            if (i == p_val.nval)
                {
                msg ("pass '%s' not found in $PASS_ORDER block", 2, pass_string);
                return (-1);
                }
            else if ((i % 2) == 0) stn->pass_direction = 'F';
            else stn->pass_direction = 'R';
            }
        }

    return (0);
    }
