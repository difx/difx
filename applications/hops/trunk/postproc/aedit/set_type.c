/************************************************************************/
/*									*/
/* This routine sets up the type input parameter, searching the		*/
/* user-typed input line and accepting 0, 1 or 2 numeric type   	*/
/* identifiers.   These identifiers may occur ANYWHERE within the 	*/
/* typed input string.							*/
/*									*/
/*	Inputs:		arg1,arg2,remarg	Typed input strings	*/
/*									*/
/*	Output:		inp.type		source input parameter	*/
/*									*/
/* Created 12 April 1989 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_type(arg1,arg2,remarg)
char *arg1, *arg2, *remarg;
    {
    extern struct inputs inp;
    char outbuf[11], buf[150];
    int l;

    outbuf[0] = '\0';

    sprintf(buf,"%s%s%s",arg1,arg2,remarg);

    if(strchr(buf,'0') != NULL) strcat(outbuf,"0,");
    if(strchr(buf,'1') != NULL) strcat(outbuf,"1,");
    if(strchr(buf,'2') != NULL) strcat(outbuf,"2,");
    if(strchr(buf,'3') != NULL) strcat(outbuf,"3,");
    if(strchr(buf,'4') != NULL) strcat(outbuf,"4,");
    l = strlen(outbuf);
    if(l != 0) outbuf[l-1] = '\0';		/* Erase last comma */
					/* Blank means pass-all, so insert 0,1,2,3,4 */
    if(l == 0) 
	{
	if (strlen(buf) > 0) 
	    msg("WARNING.  No type identifiers found in input (0,1,2,3 or 4)",2);
	else
	    sprintf (outbuf, "0,1,2,3,4");
	}
    strcpy(inp.type,outbuf);
    return(0);
    }
