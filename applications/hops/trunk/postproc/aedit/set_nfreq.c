/************************************************************************/
/*									*/
/* This routine sets up the nfreq input parameter			*/
/*									*/
/*	Inputs:		arg1,arg2,remarg	Typed input strings	*/
/*									*/
/*	Output:		inp.nfreq		fraction input value	*/
/*									*/
/* Created 19 April 1990 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_nfreq(arg1,arg2,remarg)
char *arg1, *arg2, *remarg;
{
	extern struct inputs inp;
	char buf[150], suffix[100];
	int value1, value2, greater, less_than, equal, out_of_range, n;
	int badly_formed, temp;

	greater = FALSE; less_than = FALSE; equal = FALSE;
	out_of_range = FALSE; badly_formed = FALSE;

	sprintf(buf,"%s%s%s",arg1,arg2,remarg);		/* Jam arguments together */

	n = strlen(buf);				/* Check for unset command */
	if(n == 0 || (buf[0] == '0' && n == 1)) {
	    inp.nfreq[0] = 1;
	    inp.nfreq[1] = MAXFREQ;
	    return(0);
	}

	if(buf[0] == '>') {				/* Look for inequalities */
	    greater = TRUE;
	    buf[0] = ' ';
	}
	else if(buf[0] == '<') {
	    less_than = TRUE;
	    buf[0] = ' ';
	}
	if(buf[1] == '=' && (greater || less_than)) {	/* Check for <=, >= */
	    equal = TRUE;
	    buf[1] = ' ';
	}

	n = strlen(buf);				/* Should only be number */
							/* or "-" left */
	if(n != strspn(buf," 0123456789-")) {
	    msg("Incomprehensible NFREQ specification.  See help file for format.",2);
	    return(-1);
	}

	n = sscanf(buf,"%d-%d",&value1,&value2);	/* Range is 1-14 */
	if (n == 1) 
	{
	    value2 = MAXFREQ;
	    if(!greater && !less_than) value2 = value1;
	    if(value1 < 1 || value1 > MAXFREQ) out_of_range = TRUE;
	}
	else if (n == 2)
	{
	    if(value1 < 1 || value1 > MAXFREQ) out_of_range = TRUE;
	    if(value2 < 1 || value2 > MAXFREQ) out_of_range = TRUE;
	    if(greater || less_than) badly_formed = TRUE;
	    if(value1 > value2)
	    {
		temp = value1;
		value1 = value2;
		value2 = temp;
	    }
	}
	else badly_formed = TRUE;

	if(greater)
	{
	    if(! equal) value1 += 1;
	    value2 = MAXFREQ;
	    if (value1 > MAXFREQ) out_of_range = TRUE;
	}
	if(less_than) 
	{
	    if(! equal) value1 -= 1;
	    value2 = value1;
	    value1 = 1;
	    if(value2 < 1) out_of_range = TRUE;
	}

	if(out_of_range) 
	{
	    msg("Out of range NFREQ specification.  See help file for format.",2);
	    return(-1);
	}
	if(badly_formed) 
	{
	    msg("Badly formed NFREQ specification.  See help file for format.",2);
	    return(-1);
	}

	inp.nfreq[0] = value1;			/* OK, stuff it in */
	inp.nfreq[1] = value2;

	return(0);
}
