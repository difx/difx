/************************************************************************/
/*									*/
/* This routine sets up the baselines input parameter, parsing the	*/
/* user-typed input line and accepting only 2-character alphabetic	*/
/* baseline specifiers.							*/
/*									*/
/*	Inputs:		arg1,arg2,remarg	Typed input strings	*/
/*									*/
/*	Output:		inp.baselines		baseline input par	*/
/*									*/
/* Created 12 April 1989 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int set_baselines(char *arg1, char *arg2, char *remarg)
{
	extern struct inputs inp;
	char outbuf[150], buf[150];
	char *baseline, *strtok();

	outbuf[0] = '\0';

	sprintf(buf,"%s %s %s",arg1,arg2,remarg);

	baseline = strtok(buf," ,");	/* Step through baselines */
	while(baseline != NULL) {
	    if(strlen(baseline) != 2 || (! isalpha(baseline[0])) ||
			(! isalpha(baseline[1]))) {
		msg("Bad baseline '%s'",2,baseline);
		return(-1);
	    }
	    strcat(outbuf,baseline);	/* Build up pretty baseline string */
	    strcat(outbuf," ");
	    baseline = strtok(NULL," ,");
	}
	inp.baselines[0] = '\0';
	strcpy(inp.baselines,outbuf);	/* Blank input line gives blank result */
	return(0);
}
