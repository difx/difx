/************************************************************************/
/*									*/
/* This routine sets up the prange input parameter			*/
/*									*/
/*	Inputs:		arg1,arg2,remarg	Typed input strings	*/
/*									*/
/*	Output:		inp.parameter		parameter input value	*/
/*									*/
/* Created 23 December 1993 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"
#include "usearray.h"

int
set_prange (arg1,arg2,remarg)
char *arg1, *arg2, *remarg;
    {
    char buf[150], dummy[100];
    int pnum, limits, bad, n, greater, less_than;
    float value1, value2;
    extern struct inputs inp;
    extern struct usearray user_param;
    
    greater = FALSE; less_than = FALSE; limits = FALSE;

    bad = FALSE;
    if (sscanf (arg1, "%d", &pnum) != 1) bad = TRUE;
    else if ((pnum < 0) || (pnum > user_param.nparms)) bad = TRUE;
    if (bad)
	{
	msg ("1st argument of PRANGE must be a parameter tag.  To", 3);
	msg ("identify valid tags, use the PLIST command", 3);
	return (1);
	}
					/* Jam remaining arguments together */
    sprintf(buf,"%s %s",arg2,remarg);
					/* Check for unset command */
    n = strlen(buf);
    if(n == 0 || (buf[0] == '0' && n == 1)) 
	{
					/* id tags 1-relative, this means N/A */
	inp.parameter[0] = 0.0;
	return(0);
	}
					/* 1st char may be inequality */
    if(buf[0] == '>') greater = TRUE;
    else if(buf[0] == '<') less_than = TRUE;
    else limits = TRUE;
					/* Clear for sscanf */
    if (! limits) buf[0] = ' ';
					/* Up to two f.p. args, more is error */
    n = sscanf (buf, "%f %f %s", &value1, &value2, dummy);
    if (limits && (n != 2)) bad = TRUE;
    if ((! limits) && (n != 1)) bad = TRUE;
    if (bad)
	{
	msg ("Incorrect syntax for PRANGE command (see HELP PRANGE)", 2);
	return (1);
	}
					/* Now actually set values */
    if (greater)
	{
	inp.parameter[1] = value1;
	inp.parameter[2] = 1.0e30;
	}
    else if (less_than)
	{
	inp.parameter[1] = -1.0e30;
	inp.parameter[2] = value1;
	}
    else if (limits)
	{
	if (value2 < value1)
	    {
	    msg ("Error specifying parameter limits.  Specify range with lower", 2);
	    msg ("bound first, then upper bound", 2);
	    return (1);
	    }
	inp.parameter[1] = value1;
	inp.parameter[2] = value2;
	}
    inp.parameter[0] = pnum;

    return(0);
    }
