/************************************************************************/
/*									*/
/* Takes a VEX ra string and fills in part of a HOPS sky_coord struct	*/
/* from it.								*/
/*									*/
/*	Inputs:		rastring	##h##m##.###s			*/
/*									*/
/*	Output:		coord		HOPS struct partially filled in	*/
/*									*/
/* Created November 12 1998 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "mk4_typedefs.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

int
parse_ra (char *rastring,
          struct sky_coord *coord)
    {
    int n, hour, minute, bad_range;
    double second;

					/* Scan it */
    n = sscanf (rastring, "%dh%dm%lfs", &hour, &minute, &second);
    if (n != 3) 
	{
	msg ("Bad format in ra string '%s'", 2, rastring);
	return (-1);
	}
					/* Range-checking */
    bad_range = FALSE;
    if (second < 0.0 || second >= 60.0) bad_range = TRUE;
    if (minute < 0 || minute >= 60) bad_range = TRUE;
    if (hour < 0 || hour >= 24) bad_range = TRUE;

    if (bad_range)
	{
	msg ("RA string '%s' out of range", 2, rastring);
	return (-2);
	}
					/* All is OK, fill in struct */
    coord->ra_hrs = hour;
    coord->ra_mins = minute;
    coord->ra_secs = second;

    return (0);
    }
