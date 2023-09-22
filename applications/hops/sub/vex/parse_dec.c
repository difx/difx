/************************************************************************/
/*                                                                      */
/* Takes a VEX dec string and fills in part of a HOPS sky_coord struct  */
/* from it.                                                             */
/*                                                                      */
/*      Inputs:         decstring       ##h##m##.###s                   */
/*                                                                      */
/*      Output:         coord           HOPS struct partially filled in */
/*                                                                      */
/* Created November 12 1998 by CJL                                      */
/* Fix 0 dec bug, October 25 2001, CJL                                  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_typedefs.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

int
parse_dec (char *decstring,
           struct sky_coord *coord)
    {
    int n, deg, minute, bad_range, negative;
    double second;

                                        /* Scan it */
    negative = FALSE;
    if (decstring[0] == '-') negative = TRUE;
    n = sscanf (decstring, "%dd%d'%lf\"", &deg, &minute, &second);
    if (n != 3) 
        {
        msg ("Bad format in dec string '%s'", 2, decstring);
        return (-1);
        }
                                        /* Range-checking */
    bad_range = FALSE;
    if (second < 0.0 || second >= 60.0) bad_range = TRUE;
    if (minute < 0 || minute >= 60) bad_range = TRUE;
    if (deg < -90 || deg > 90) bad_range = TRUE;
    if ((deg == -90) || (deg == 90))
        if ((minute > 0) || (second > 0.0)) bad_range = TRUE;

    if (bad_range)
        {
        msg ("DEC string '%s' out of range", 2, decstring);
        return (-2);
        }
                                        /* All is OK, fill in struct */
                                        /* Take care of -00 dec */
    if (negative && (deg == 0))
        {
        minute = -minute;
        second = -second;
        }
    coord->dec_degs = deg;
    coord->dec_mins = minute;
    coord->dec_secs = second;

    return (0);
    }
