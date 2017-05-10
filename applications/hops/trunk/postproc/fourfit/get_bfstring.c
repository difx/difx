/************************************************************************/
/*									*/
/* Processes the -b option from the command line.  The user specifies	*/
/* an argument of the general form "BB:F", meaning "baseline:fgroup",	*/
/* where baseline can be wildcarded or just one station.  Also either	*/
/* baseline or fgroup can be omitted.  The routine returns a string	*/
/* in fourfit control file syntax which has the desired effect.		*/
/*									*/
/*	Inputs:		barg		straight from command line	*/
/*									*/
/*	Output:		return value	control syntax, or zero-length	*/
/*					string if error			*/
/*									*/
/* Created 27 January 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>

#define FALSE 0
#define TRUE 1

char *
get_bfstring (char* barg)

    {
    int i, fgroup, fcount, bcount;
    char c, fgval, baseline[5], bstring[30], fstring[30];
    static char outstring[60];

    bstring[0] = fstring[0] = outstring[0] = '\0';

    if (strlen (barg) > 4)
	{
	msg ("illegal -b argument '%s', see fourfit.doc for syntax", 3, barg);
	return (outstring);
	}
					/* Pick out the pieces */
    i = bcount = fcount = 0;
    fgroup = FALSE;
    while ((c = barg[i++]) != '\0')
	{
	if (c == ':') fgroup = TRUE;
	else if (fgroup)
	    {
	    fgval = c;
	    fcount++;
	    }
	else baseline[bcount++] = c;
	}
    baseline[bcount] = '\0';
					/* Another check for crap */
    if ((bcount > 2) || (fcount > 1) || ((bcount == 0) && (fcount == 0)))
	{
	msg ("illegal -b argument '%s', see fourfit.doc for syntax", 3, barg);
	return (outstring);
	}
					/* OK, make the string */
    switch (bcount)
	{
	case 0:
	    break;
	case 1:
	    sprintf (bstring, "if not station %c skip true", baseline[0]);
	    break;
	case 2:
	    sprintf (bstring, "if not baseline %s skip true", baseline);
	    break;
	default:
	    ;
	}
    if (fcount == 1) sprintf (fstring, "if not f_group %c skip true", fgval);

    sprintf (outstring, "%s %s", bstring, fstring);

    return (outstring);
    }
