/********************************************************************************/
/*										*/
/* This routine interprets all alphabetic characters in the argument		*/
/* list as stations, and overwrites the inputs value with these station		*/
/* id's.  Duplicates are ignored.						*/
/*										*/
/*	Inputs:		arg1, arg2, remarg	All text typed after		*/
/*						STATIONS command		*/
/*										*/
/*	Output:		inp.stations		One of the input		*/
/*						data selecion parameters	*/
/*										*/
/* Created April 3 1989 by CJL							*/
/*										*/
/********************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int set_stations(char *arg1, char *arg2, char *remarg)
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
		    if(j > 13) {			/* Check for overflow */
			msg("Too many stations listed",2);
			return(-1);
		    }
		    outbuf[j++] = c;
		    outbuf[j] = '\0';
		}
	    }
	}

	strcpy(inp.stations, outbuf);	/* Copy to inp structure */
	return(0);
}
