/************************************************************************/
/*									*/
/* Checks the input filenames to see if the latter is a valid child	*/
/* file of the former parent root file.  It does this by first looking	*/
/* at the root code extension, then by verifying that both files	*/
/* reside in the same directory.					*/
/*									*/
/*	Inputs:		rootname, dataname	as on command line	*/
/*									*/
/*	Output:		return value		1=match			*/
/*						0=no match		*/
/*									*/
/* Created 23 September 1992 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mk4_util.h"

int
root_belong(char *rootname, char *dataname)
    {
    int i, rlen, dlen, rcompare, dcompare, ret;
    char c;

    rlen = strlen (rootname);
    dlen = strlen (dataname);
					/* First check root id, this is fast */
    if (strcmp (rootname+rlen-6, dataname+dlen-6) != 0) return (0);

					/* Now must find last occurrence of '/' in */
					/* each filename so can compare directories */
    rcompare = dcompare = -1;
    i = 0;
    while ((c = rootname[i++]) != '\0')
	if (c == '/') rcompare = i - 1;
    i = 0;
    while ((c = dataname[i++]) != '\0')
	if (c == '/') dcompare = i - 1;
					/* Unequal lengths --> mismatch, both zero */
					/* lengths --> match, otherwise do comparison */
    if (rcompare != dcompare) ret = 0;
    else if (rcompare <= 0) ret = 1;
    else if (strncmp (rootname, dataname, rcompare) == 0) ret = 1;
    else ret = 0;
					/* This means bad directory, worthy of mention */
					/* to the user */
    if (ret == 0)
	msg ("Warning, %s not in same directory as %s", 2, dataname+dcompare+1,
								rootname+rcompare+1);
					/* Send back answer */
    return (ret);
    }
