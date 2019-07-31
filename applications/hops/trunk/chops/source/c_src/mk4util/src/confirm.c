/************************************************************************/
/*									*/
/* This routine prompts the user for confirmation, typically before	*/
/* performing an operation that has been determined to be questionable.	*/
/*									*/
/*	Inputs:		string		Message string			*/
/*									*/
/*	Output:		return value	TRUE or FALSE			*/
/*									*/
/* Created 5 October 1992 by CJL					*/
/* Added toggle for non-interactive mode for use by any program ... 	*/
/* looks for string ON or OFF as argument, CJL December 15 1993		*/
/*									*/
/************************************************************************/

#include <string.h>
#include <stdio.h>
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

int
confirm (char *string)
    {
    char buf[100];
    int max = 10;
    extern char progname[];		/* *progname no good here */
    static int interactive = TRUE;

    if (strcmp (string, "OFF") == 0) 
	{
	interactive = FALSE;
	return (TRUE);
	}
    if (strcmp (string, "ON")  == 0) 
	{
	interactive = TRUE;
	return (TRUE);
	}
					/* Batch mode, always do it */
    if (! interactive) return (TRUE);

    printf("%s: %s (y/n) : ", progname, string);
    while(max-- > 0) 
	{
	if (!fgets (buf, 99, stdin)) break;
	if(buf[0] == 'Y' || buf[0] == 'y') return(TRUE);
	else if(buf[0] == 'N' || buf[0] == 'n') return(FALSE);
	else printf("Answer 'y' or 'n' : ");
	}
					/* EOF or incompetant user */
    return(FALSE);
    }

/*
 * eof
 */
