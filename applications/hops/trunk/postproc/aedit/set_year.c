/************************************************************************/
/*									*/
/* This is a kludge to get around nasty A-file versions that lack a	*/
/* scan year field.  This makes for nasty timeranges, so we alert the	*/
/* user to the problem in read_data.c, and invite him/her to manually	*/
/* edit the year with the "setyear" command.  Loops through all data	*/
/* resetting the year, so you must edit out all data with valid years	*/
/* first.								*/
/*									*/
/*	Inputs:		arg1		string with desired year	*/
/*									*/
/*	Output:		return value	0 good, -1 bad			*/
/*									*/
/* Created 24 April 1989 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "aedit.h"

#define TRUE 1
#define FALSE 0

int set_year (fringearray *fdata, char *arg1)
{
    extern int fscan, up_to_date;
    int year, badyear, day, hour, min, sec, i, n;

    if(sscanf(arg1,"%d",&year) != 1) 	/* Check for integer */
	{
	msg("Bad year specified",2);
	return(-1);
	}

    year %= 100;
    if(year < 80) 				/* Check for user brain cells */
	{
	msg("Bad year specified",2);
	return(-1);
	}

    n = 0;					/* Overwrite year in all */
    for(i=0;i<fscan;i++) 			/* unflagged data */
	{
	if(fdata[i].flag == 0) 
	    {
	    int_to_time (fdata[i].data.time_tag, &badyear, &day, &hour, &min, &sec);
	    fdata[i].data.time_tag = time_to_int (year, day, hour, min, sec);
	    n++;
	    }
	}

    if(n > 0) 
	{
	msg("Reset year for %d scans",2,n);
	up_to_date = FALSE;
	}
    return(0);
    }
