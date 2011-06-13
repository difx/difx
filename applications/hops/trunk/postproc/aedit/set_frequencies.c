/********************************************************************************/
/*										*/
/* This routine interprets all alphabetic characters in the argument		*/
/* list as frequencies, and overwrites the inputs value with these 		*/
/* id's.  Duplicates are ignored.						*/
/*										*/
/*	Inputs:		arg1, arg2, remarg	All text typed after		*/
/*						STATIONS command		*/
/*										*/
/*	Output:		inp.frequencies		One of the input		*/
/*						data selecion parameters	*/
/*										*/
/* Created April 12 1989 by CJL							*/
/*										*/
/********************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_frequencies(arg1, arg2, remarg)
char *arg1, *arg2, *remarg;
{
	extern struct inputs inp;
	int l, i, j;
	char c, buf[150], outbuf[10];

	sprintf(buf,"%s%s%s",arg1,arg2,remarg);
	l = strlen(buf);
	j = 0; outbuf[0] = '\0';

	for(i=0;i<l;i++) {		/* Extract letters from all arguments */
	    c = buf[i];
	    if(isalpha(c)) {
		if(strchr(outbuf,c) == NULL) {		/* No duplicates */
		    if(j > 8) {				/* Check for overflow */
			msg("Too many frequencies listed",2);
			return(-1);
		    }
		    outbuf[j++] = c;
		    outbuf[j] = '\0';
		}
	    }
	}

	strcpy(inp.frequencies, outbuf);	/* Copy to inp structure */
	return(0);
}
