/************************************************************************/
/*									*/
/* This routine takes the start and end addresses of a section of VEX	*/
/* text, and the address of a character array.  It then copies the vex	*/
/* code into the character array, omitting comments and excess		*/
/* whitespace.  The resulting string is then ready for simple parsing.	*/
/*									*/
/*	Inputs:		first		Start address of text		*/
/*			last		End address of text		*/
/*									*/
/*	Output:		str		Stripped output string		*/
/*			return value	0=OK, else bad			*/
/*									*/
/* Created 24 August 1998 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
strip_text (char *first,
            char *last,
            char *str)
    {
    int i, nc, last_white, first_comment, last_comment;
    char *start, *end, *ptr;
    extern char *vexstart, *vexend;
    extern struct comment clist[];
    extern int ncom;
					/* Input check */
    if ((first < vexstart) || (last > vexend))
	{
	msg ("Invalid addresses passed to strip_text()", 2);
	return (-1);
	}
					/* Make list of relevant comment */
					/* strings */
    first_comment = last_comment = -1;
    for (i=0; i<ncom; i++)
	{
	if (clist[i].end < first) continue;
	if (clist[i].start > last) continue;
	if (first_comment == -1) first_comment = i;
	last_comment = i;
	}
					/* Copy character by character */
					/* Set so that leading whitespace */
					/* is deleted */
    last_white = TRUE;
    nc = 0;
    for (ptr=first; ptr<=last; ptr++)
	{
					/* Are we in one of the comment */
					/* strings? */
	if (last_comment >= 0)
	    {
	    for (i=first_comment; i<=last_comment; i++)
		{
		start = clist[i].start;
		end = clist[i].end;
					/* If at start, skip to end */
		if (ptr == start) 
		    {
		    ptr = end;
		    break;
		    }
		}
	    }
					/* Copy whitespace characters as */
					/* spaces, only if the last character */
					/* copied was not whitespace */
	switch (*ptr)
	    {
	    case ' ':
	    case '\t':
	    case '\n':
	    case '\r':
					/* but copy quoted strings verbatim */
		if (in_quote (ptr)) str[nc++] = *ptr;
		else if (! last_white) 
		    {
		    str[nc++] = ' ';
		    last_white = TRUE;
		    }
		break;
					/* Copy all others as is */
	    default:
		str[nc++] = *ptr;
		last_white = FALSE;
		break;
	    }
					/* Did we just exceed string length? */
	if (nc >= STATEMENT_SIZE)
	    {
	    msg ("Error - maximum statement size exceeded", 2);
	    str[0] = '\0';
	    return (-1);
	    }
	}
					/* Strip trailing space */
    if (str[nc-1] == ' ') nc--;
					/* Null terminate */
    str[nc] = '\0';

    return (0);
    }
