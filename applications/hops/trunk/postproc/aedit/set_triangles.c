/************************************************************************/
/*									*/
/* This routine sets up the triangles input parameter, parsing the	*/
/* user-typed input line and accepting only 3-character alphabetic	*/
/* triangle specifiers.							*/
/*									*/
/*	Inputs:		arg1,arg2,remarg	Typed input strings	*/
/*									*/
/*	Output:		inp.triangles		triangle input parm	*/
/*									*/
/* Created 29 August 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_triangles (arg1, arg2, remarg)
char *arg1, *arg2, *remarg;
    {
    extern struct inputs inp;
    char outbuf[250], buf[250];
    char *triangle, *strtok();

    outbuf[0] = '\0';

    sprintf (buf, "%s %s %s", arg1, arg2, remarg);
					/* Step through triangles */
    triangle = strtok (buf, " ,");
    while (triangle != NULL) 
	{
	if (strlen (triangle) != 3 || (! isalpha (triangle[0])) ||
			(! isalpha (triangle[1])) || (! isalpha (triangle[2]))) 
	    {
	    msg ("Bad triangle '%s'", 2, triangle);
	    return (-1);
	    }
					/* Build up pretty triangle string */
	strcat (outbuf, triangle);
	strcat (outbuf, " ");
	triangle = strtok (NULL, " ,");
	}
    inp.triangles[0] = '\0';
    strcpy (inp.triangles, outbuf);	/* Blank input line gives blank result */
    return(0);
    }
