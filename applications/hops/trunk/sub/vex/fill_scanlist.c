/************************************************************************/
/*									*/
/* This routine will locate all the scans in the $SCHED block, placing	*/
/* the information in the struct array slist.				*/
/*									*/
/*	Inputs:		blist		via extern from locate_blocks()	*/
/*			slist		via extern			*/
/*									*/
/*	Output:		slist (extern)	Filled in appropriately		*/
/*			return value	0=OK, else bad			*/
/*									*/
/* Created 25 August 1998 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define SCAN 1
#define ENDSCAN 2
#define OTHER 3

int
fill_scanlist (void)
    {
    int i, start, end, in_scan, stmt, st, n;
    char stname[128], arg1[128], junk[256];
    extern int nblock, nstmt, nscan;
    extern struct block blist[];
    extern struct statement *stlist;
    extern struct scan slist[];
					/* Get start/end statement #'s */
					/* of $SCHED block */
    for (i=0; i<nblock; i++)
	{
	if (strcmp ("SCHED", blist[i].name) != 0) continue;
	start = blist[i].stno;
	if (i == nblock-1) end = nstmt;
	else end = blist[i+1].stno;
	break;
	}
    if (i == nblock)
	{
	msg ("Error, $SCHED not found!", 2);
	return (-1);
	}
					/* Search for scan/endscan pairs */
    in_scan = FALSE;
    nscan = 0;
    for (st=start+1; st<end; st++)
	{
	if (nscan >= MAXSCANS)
	    {
	    msg ("Too many scans in $SCHED block", 2);
	    return (-1);
	    }
					/* Identify the statement */
	n = sscanf (stlist[st].str, "%s %s %s", stname, arg1, junk);
	if (strcmp (stname, "scan") == 0) stmt = SCAN;
	else if (strcmp (stname, "endscan") == 0) stmt = ENDSCAN;
	else stmt = OTHER;
					/* Check for rogue statements */
	if ((! in_scan) && (stmt != SCAN))
	    {
	    msg ("Inappropriate '%s' statement outside scan", 2, stname);
	    print_location (st);
	    return (-1);
	    }
	if ((in_scan) && (stmt == SCAN))
	    {
	    msg ("Missing 'endscan' for scan '%s'", 2, slist[nscan].name);
	    print_location (st);
	    return (-1);
	    }
					/* Take appropriate action */
	switch (stmt)
	    {
	    case SCAN:
					/* Valid scan has 1 argument */
		if (n != 2)
		    {
		    msg ("Malformed scan statement '%s'", 2, stlist[st].str);
		    print_location (st);
		    return (-1);
		    }
		slist[nscan].start = st;
		strncpy (slist[nscan].name, arg1, MAX_NAMESIZE);
					/* Check for duplicate scan name */
		for (i=0; i<nscan; i++)
		    {
		    if (strcmp (slist[i].name, slist[nscan].name) != 0)
			continue;
		    msg ("Duplicate scan names '%s'", 2, slist[nscan].name);
		    print_location (st);
		    return (-1);
		    }
					/* All OK */
		in_scan = TRUE;
		break;

	    case ENDSCAN:
		if (n != 1)
		    {
		    msg ("Malformed endscan statement '%s'", 2, stlist[st].str);
		    print_location (st);
		    return (-1);
		    }
		slist[nscan].end = st;
		nscan++;
		in_scan = FALSE;
		break;

	    case OTHER:
	    default:
		break;
	    }
	}
					/* Better have finished with */
					/* an enddef */
    if (in_scan)
	{
	msg ("Missing 'endscan' for scan '%s'", 2, slist[nscan].name);
	print_location (st);
	return (-1);
	}

    return (0);
    }
