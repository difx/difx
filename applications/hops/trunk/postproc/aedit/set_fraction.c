/************************************************************************/
/*									*/
/* This routine sets up the fraction input parameter			*/
/*									*/
/*	Inputs:		arg1,arg2,remarg	Typed input strings	*/
/*									*/
/*	Output:		inp.fraction		fraction input value	*/
/*									*/
/* Created 19 April 1990 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_fraction(arg1,arg2,remarg)
char *arg1, *arg2, *remarg;
{
	extern struct inputs inp;
	char buf[150], suffix[100];
	int value, greater, less_than, equal, garbled, percent, n;

	greater = FALSE; less_than = FALSE; equal = FALSE;
	garbled = FALSE; percent = FALSE; value = -20; suffix[0] = '\0';

	sprintf(buf,"%s%s%s",arg1,arg2,remarg);		/* Jam arguments together */

	n = strlen(buf);				/* Check for unset command */
	if(n == 0 || (buf[0] == '0' && n == 1)) {
	    inp.fraction = 0;
	    return(0);
	}

	if(buf[0] == '>') greater = TRUE;		/* 1st char should be ineq. */
	else if(buf[0] == '<') less_than = TRUE;
	else garbled = TRUE;
	buf[0] = ' ';					/* Clear for sscanf */
	if(buf[1] == '=') {				/* Check for <=, >= */
	    equal = TRUE;
	    buf[1] = ' ';
	}
	sscanf(buf,"%d%s",&value,suffix);		/* Form is [0-10] or [0-100]% */
	if(suffix[0] == '%') percent = TRUE;
	if(percent) value = (value+5) / 10;		/* Convert % to 10ths */
	if(value < 0) garbled = TRUE;
	if(greater && (! equal)) value += 1;
	else if(less_than && (! equal)) value = -value + 1;
	else if(less_than && equal) value = -value;
	if(value > 9 || value < -9) garbled = TRUE;	/* Out of range */
	if((! percent) && strlen(suffix) > 0) garbled = TRUE;	/* Bad suffix */

	if(garbled) {
	    msg("Incomprehensible FRACTION option.  See help file for format.",2);
	    return(-1);
	}
	else inp.fraction = value;			/* OK, stuff it in */
	return(0);
}
