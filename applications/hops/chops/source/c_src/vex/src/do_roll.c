/************************************************************************/
/*									*/
/* Given a roll def and a station parameter structure, this routine 	*/
/* extracts and stores all the relevant information			*/
/*									*/
/*	Inputs:		dl              pointer to def_list struct      */
/*									*/
/*	Output:		stn		The station struct to be filled	*/
/*			return value	0=OK, else bad			*/
/*									*/
/* Created January 13, 1998 by CJL					*/
/* Rewritten for Haystack parser, 2 November 1998 by CJL                */
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
do_roll (struct def_list *dl,
         struct station_struct *stn)
    {
    int i, st, hs, home;
    char *str, *ptr;
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
	if (parse_pval (str, "ROLL", OVEX | ovex_ver, &p_val) != 0)
	    {
	    msg ("Error parsing statement '%s'", 2, stlist[st].str);
	    print_location (st);
	    return (-1);
	    }

	if ISNAME ("roll")
	    {
	    ptr = p_val.dval[0].data.strval;
	    if (strcmp (ptr, "on") == 0) stn->roll = ON;
	    else if (strcmp (ptr, "off") == 0) stn->roll = OFF;
	    }
	else if ISNAME ("roll_inc_period")
	    stn->roll_increment = p_val.dval[0].data.intval;
	else if ISNAME ("roll_reinit_period")
	    stn->roll_period = p_val.dval[0].data.realval;
	else if ISNAME ("roll_def")
	    {
	    if (p_val.nval > 32)
		{
		msg ("Too many $ROLL entries", 2);
		return (-1);
		}
	    hs = p_val.dval[0].data.intval;
	    home = p_val.dval[1].data.intval;
	    for (i=2; i<p_val.nval; i++)
		stn->roll_seq[hs][home][i-2] = p_val.dval[i].data.intval;
	    }
	}

    return (0);
    }
