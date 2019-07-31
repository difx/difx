/************************************************************************/
/*									*/
/* This routine sets up the mode plotting input parameter.  It accepts	*/
/* a single argument "split" or "nosplit", setting the value of the 	*/
/* inp.mode parameter to 0 or 1 respectively.				*/
/*									*/
/*	Inputs:		arg1			Typed input strings	*/
/*									*/
/*	Output:		inp.mode		Mode input parameter	*/
/*									*/
/* Created 23 April 1990 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int set_mode(char *arg1)
{
	extern struct inputs inp;
	int i, n;
	char c;

	n = strlen(arg1);
	for(i=0;i<n;i++) {
	    c = arg1[i];
	    if(isupper(c)) c = tolower(c);
	    arg1[i] = c;
	}
	if(strncmp(arg1,"split",n) == 0) inp.mode = 1;
	else if(strncmp(arg1,"nosplit",n) == 0) inp.mode = 0;
	else return(-1);
	return(0);
}
