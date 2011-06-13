/************************************************************************/
/*									*/
/* Takes a VEX date string and fills in a HOPS date structure from it	*/
/*									*/
/*	Inputs:		datestring	##y##d##h##m##.##s		*/
/*									*/
/*	Output:		date		HOPS struct filled in		*/
/*									*/
/* Created December 16 1997 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "mk4_typedefs.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

int
parse_date (char *datestring,
            struct date *date)
    {
    int n, year, day, hour, minute, bad_range;
    double second;

					/* Scan it, allowing truncated forms */
    n = sscanf (datestring, "%dy%dd%dh%dm%lfs", 
			&year, &day, &hour, &minute, &second);
    if (n < 1) 
	{
	msg ("Unparseable date string '%s'", 2, datestring);
	return (-1);
	}
    if (n < 2) day = 1;
    if (n < 3) hour = 0;
    if (n < 4) minute = 0;
    if (n < 5) second = 0.0;
					/* Range-checking */
    bad_range = FALSE;
    if (second < 0.0 || second >= 60.0) bad_range = TRUE;
    if (minute < 0 || minute >= 60) bad_range = TRUE;
    if (hour < 0 || hour >= 24) bad_range = TRUE;
    if (day < 1 || day > 366) bad_range = TRUE;
    if (year < 80) year += 2000;
    if (year > 80 && year < 100) year += 1900;
    if (year >= 100 && year < 1980) bad_range = TRUE;

    if (bad_range)
	{
	msg ("Date string '%s' out of range", 2, datestring);
	return (-2);
	}
					/* All is OK, fill in struct */
    date->year = year;
    date->day = day;
    date->hour = hour;
    date->minute = minute;
    date->second = second;

    return (0);
    }
