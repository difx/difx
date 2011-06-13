/************************************************************************/
/*									*/
/* Fills in the eop parts of the scan structure, from an eop def	*/
/*									*/
/*	Inputs:		eopdef		Pointer to the eop def		*/
/*									*/
/*	Output:		scan		Pointer to the scan struct	*/
/*			return value	0=OK, else bad			*/
/*									*/
/* Created 16 February 1998 by CJL					*/
/* Rewritten for Haystack parser 28 October 1998 by CJL                 */
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring)  (strcmp (p_val.name, namestring) == 0)

int
do_eop (struct def *eopdef,
        struct scan_struct *scan)
    {
    int st, i, nut, nxw, nyw;
    char *str;
    struct param_val p_val;
    extern struct statement *stlist;
    extern int ovex_ver;
					/* Loop over statements in def */
    nut = nxw = nyw = 0;
    for (st=eopdef->start+1; st<eopdef->end; st++)
	{
	str = stlist[st].str;
					/* Parse "parameter=" statement */
	if (parse_pval (str, "EOP", OVEX | ovex_ver, &p_val) != 0)
	    {
	    msg ("Error parsing statement '%s'", 2, stlist[st].str);
	    return (-1);
	    }
					/* Need several items, ignore rest */

	if ISNAME ("TAI-UTC") 
	    scan->tai_utc = p_val.dval[0].data.realval;
	else if ISNAME ("A1-TAI") 
	    scan->a1_tai = p_val.dval[0].data.realval;
	else if ISNAME ("eop_interval") 
	    scan->eop_interval = p_val.dval[0].data.realval;
	else if ISNAME ("eop_ref_epoch") 
	    memcpy (&(scan->eop_reftime), &(p_val.dval[0].data.epochval),
				sizeof (struct date));
	else if ISNAME ("num_eop_points") 
	    scan->neop = p_val.dval[0].data.intval;
	else if ISNAME ("ut1-utc") 
	    {
	    if (p_val.nval > 10)
		{
		msg ("Too many entries in EOP lists", 2);
		return (-1);
		}
 	    nut = p_val.nval;
	    for (i=0; i<nut; i++) 
		scan->ut1_utc[i] = p_val.dval[i].data.realval;
	    }
	else if ISNAME ("x_wobble") 
	    {
	    if (p_val.nval > 10)
		{
		msg ("Too many entries in EOP lists", 2);
		return (-1);
		}
	    nxw = p_val.nval;
	    for (i=0; i<nxw; i++) 
		scan->x_wobble[i] = p_val.dval[i].data.realval;
	    }
	else if ISNAME ("y_wobble") 
	    {
	    if (p_val.nval > 10)
		{
		msg ("Too many entries in EOP lists", 2);
		return (-1);
		}
	    nyw = p_val.nval;
	    for (i=0; i<nyw; i++) 
		scan->y_wobble[i] = p_val.dval[i].data.realval;
	    }
	}
					/* Consistency check */
    if ((nut > 0) && (nxw > 0) && (nyw > 0))
	{
	if ((nut != scan->neop) || (nxw != scan->neop) || (nyw != scan->neop))
	    {
	    msg ("Inconsistent number of EOP points given", 2);
	    return (-1);
	    }
	}

    return (0);
    }
    
