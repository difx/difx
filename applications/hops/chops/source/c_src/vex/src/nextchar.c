/************************************************************************/
/*									*/
/* Simple routine to find the next non-whitespace, non-comment		*/
/* character in a vex file, starting at the memory address ptr.		*/
/* It is assumed that the supplied pointer is not within a comment	*/
/*									*/
/*	Inputs:		ptr		current position in memory	*/
/*					resident copy of vex file	*/
/*									*/
/*	Output:		return value	next non-whitespace character	*/
/*									*/
/* Created 21 August 1998 by CJL					*/
/*									*/
/************************************************************************/
#define TRUE 1
#define FALSE 0

#include "mk4_vex.h"

char
nextchar (char *ptr)
    {
    int found;

    found = FALSE;
					/* Need to find terminating null */
					/* explicitly in case file ends */
					/* with non-newline terminated */
					/* comment */
    while (TRUE)
	{
					/* Ignore commented text */
	if (in_comment (ptr)) continue;

	switch (*ptr)
	    {
					/* Skip whitespace */
	    case ' ':
	    case '\t':
	    case '\n':
		break;
	    case '\0':
		found = TRUE;
		break;
					/* The character we are after */
	    default:
		found = TRUE;
		break;
	    }
	if (found) return (*ptr);
	ptr++;
	}
    }
