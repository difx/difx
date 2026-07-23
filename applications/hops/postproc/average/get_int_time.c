/************************************************************************/
/*									*/
/* This routine takes the user-specified integration time, and adjusts	*/
/* it to an "approved" time that fits minute/hour boundaries nicely	*/
/*									*/
/*	Inputs:		flag		User-specified option string	*/
/*									*/
/*	Output:		return value	adjusted integration time	*/
/*									*/
/* Created April 10 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <strings.h>
#include "average.h"
#include "mk4_util.h"

#define NTIMES 30

int
get_int_time (char *flag)
    {
    int n, i, number, req_time, int_time, diff1, diff2;
    char units;
    static int allowed_times[NTIMES]={10,12,15,20,30,40,60,90,120,150,
		180,240,300,360,480,600,720,900,1200,1800,2400,3000,3600,
		4800,6000,7200,9000,10800,14400,18000};
					/* Special scan averaging option */
    if (strcasecmp (flag, "scan") == 0) return (0);
					/* Decode input string */
    n = sscanf (flag, "%d%c", &number, &units);

    if (n == 0)
	{
	msg ("Invalid -i flag argument '%s'", 2, flag);
	msg ("Averaging time remains at scan length", 2);
	return (0);
	}
					/* hours, minutes or seconds? */
    else if (n == 1)
	req_time = number;
    else if ((units == 'm') || (units == 'M'))
	req_time = number * 60;
    else if ((units == 'h') || (units == 'H'))
	req_time = number * 3600;
    else
	{
	msg ("Unrecognised 'units' designator '%c'", 2, units);
	msg ("Averaging time remains at scan length", 2);
	return (0);
	}
					/* Find nearest allowed time */
					/* ends of range are special case */
    for (i=0; i<NTIMES; i++)
	if (req_time < allowed_times[i]) break;
    if (i == 0) int_time = 10;
    else if (i == NTIMES) int_time = 18000;
    else
	{
	diff1 = req_time - allowed_times[i-1];
	diff2 = allowed_times[i] - req_time;
	if (diff1 > diff2) int_time = allowed_times[i];
	else int_time = allowed_times[i-1];
	}
					/* Say what we did and exit */
    if (int_time != req_time)
	{
	msg ("Requested averaging time of %d seconds adjusted to", 2, req_time);
	msg ("nearest allowed averaging time of %d seconds", 2, int_time);
	}

    return (int_time);
    }
