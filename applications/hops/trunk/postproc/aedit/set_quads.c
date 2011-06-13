/************************************************************************/
/*									*/
/* This routine sets up the quads input parameter, parsing the		*/
/* user-typed input line and accepting only 4-character alphabetic	*/
/* quad specifiers.							*/
/*									*/
/*	Inputs:		arg1,arg2,remarg	Typed input strings	*/
/*									*/
/*	Output:		inp.quads		quad input parm		*/
/*									*/
/* Created 29 August 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_quads (arg1, arg2, remarg)
char *arg1, *arg2, *remarg;
    {
    extern struct inputs inp;
    char outbuf[250], buf[250];
    char *quad, *strtok();

    outbuf[0] = '\0';

    sprintf (buf, "%s %s %s", arg1, arg2, remarg);
					/* Step through quads */
    quad = strtok (buf, " ,");
    while (quad != NULL) 
	{
	if (strlen (quad) != 4 || 
			(! isalpha (quad[0])) || (! isalpha (quad[1])) ||
			(! isalpha (quad[2])) || (! isalpha (quad[3]))) 
	    {
	    msg ("Bad quad '%s'", 2, quad);
	    return (-1);
	    }
					/* Build up pretty quad string */
	strcat (outbuf, quad);
	strcat (outbuf, " ");
	quad = strtok (NULL, " ,");
	}
    inp.quads[0] = '\0';
    strcpy (inp.quads, outbuf);		/* Blank input line gives blank result */
    return(0);
    }
