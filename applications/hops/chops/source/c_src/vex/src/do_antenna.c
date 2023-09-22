/************************************************************************/
/*                                                                      */
/* Given an antenna def and a station parameter structure, this routine */
/* extracts and stores all the relevant information                     */
/*                                                                      */
/*      Inputs:         dl              pointer to def_list struct      */
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
do_antenna (struct def_list *dl,
            struct station_struct *stn)
    {
    int st;
    char *str, ax1[128], ax2[128];
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
        if (parse_pval (str, "ANTENNA", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }

        if ISNAME ("axis_type")
            {
            strcpy (ax1, p_val.dval[0].data.strval);
            strcpy (ax2, p_val.dval[1].data.strval);
            if ((strcmp (ax1, "ha") == 0) && (strcmp (ax2, "dec") == 0))
                stn->axis_type = EQUATORIAL;
            else if ((strcmp (ax1, "x") == 0) && (strcmp (ax2, "yns") == 0))
                stn->axis_type = X_YNS;
            else if ((strcmp (ax1, "az") == 0) && (strcmp (ax2, "el") == 0))
                stn->axis_type = AZEL;
            else if ((strcmp (ax1, "x") == 0) && (strcmp (ax2, "yew") == 0))
                stn->axis_type = X_YEW;
            else
                {
                msg ("Unrecognized axis type '%s:%s'", 2, ax1, ax2);
                return (-1);
                }
            }

        else if ISNAME ("axis_offset") 
            stn->axis_offset = p_val.dval[1].data.realval;
        }

    return (0);
    }
