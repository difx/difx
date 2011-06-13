/************************************************************************/
/*									*/
/* Given a site def and a station parameter structure, this routine 	*/
/* extracts and stores all the relevant information			*/
/*									*/
/*	Inputs:		dl              pointer to def_list struct      */
/*									*/
/*	Output:		stn		The station struct to be filled	*/
/*			return value	0=OK, else bad			*/
/*									*/
/* Created January 12, 1998 by CJL					*/
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
do_site (struct def_list *dl,
         struct station_struct *stn)
    {
    int st;
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
	if (parse_pval (str, "SITE", OVEX | ovex_ver, &p_val) != 0)
	    {
	    msg ("Error parsing statement '%s'", 2, stlist[st].str);
	    print_location (st);
	    return (-1);
	    }

	if ISNAME ("zen_atmos")
	    stn->zenith_atm = p_val.dval[0].data.realval;
	else if ISNAME ("site_type")
	    {
	    ptr = p_val.dval[0].data.strval;
	    if (strcmp (ptr, "fixed") == 0) stn->site_type = FIXED;
	    else if (strcmp (ptr, "earth_orbit") == 0)
		stn->site_type = EARTH_ORBIT;
	    }
	else if ISNAME ("site_name")
	    strcpy (stn->site_name, p_val.dval[0].data.strval);
	else if ISNAME ("site_ID")
	    strcpy (stn->site_id, p_val.dval[0].data.strval);
	else if ISNAME ("mk4_site_ID")
	    stn->mk4_site_id = p_val.dval[0].data.strval[0];
	else if ISNAME ("site_position_epoch")
	    memcpy (&(stn->coordinate_epoch), &(p_val.dval[0].data.epochval),
			sizeof (struct date));
	else if ISNAME ("occupation_code")
	    strcpy (stn->occucode, p_val.dval[0].data.strval);
	else if ISNAME ("site_position")
	    {
	    stn->coordinates[0] = p_val.dval[0].data.realval;
	    stn->coordinates[1] = p_val.dval[1].data.realval;
	    stn->coordinates[2] = p_val.dval[2].data.realval;
	    }
	else if ISNAME ("site_velocity")
	    {
	    stn->site_velocity[0] = p_val.dval[0].data.realval;
	    stn->site_velocity[1] = p_val.dval[1].data.realval;
	    stn->site_velocity[2] = p_val.dval[2].data.realval;
	    }
	}

    return (0);
    }
