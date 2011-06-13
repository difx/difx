/************************************************************************/
/*									*/
/* Writes out an informative header, so the user can figure out what's	*/
/* in a file at a later date.						*/
/*									*/
/*	Inputs:		fpout		open output stream		*/
/*			fields		structure array with the info	*/
/*									*/
/*	Output:		return value	0 is OK, !=0 is bad		*/
/*									*/
/* Created 3 March 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include "adump.h"
#include "mk4_util.h"
#include "mk4_afio.h"

int
write_header (FILE *fpout,
              struct flist fields[])
    {
    int i, ret;
    char field_list[512];
    time_t now;
/*  extern int errno, sys_nerr; */
/*  extern char *sys_errlist[]; */

    now = time (NULL);
    ret = fprintf (fpout, "* This file written by adump, %s", ctime (&now));
    if (ret < 0) return (1);
    ret = fprintf (fpout, "* Fields list:\n");
    if (ret < 0) return (1);
					/* Construct the list of fields */
    i = 0;
    strcpy (field_list, "* ");
    while (fields[i].id > 0)
	{
	strcat (field_list, fields[i].name);
	strcat (field_list, " ");
	i++;
	}
					/* Tack on newlines and */
					/* write it out */
    strcat (field_list, "\n*\n");
    if (fputs (field_list, fpout) == EOF)
	{
	msg ("Error writing header line", 3);
	/* if (errno <= sys_nerr)
	    msg ("Error writing header line: %s", 3, sys_errlist[errno]);
	else  msg ("Error writing header line", 3); */
	return (1);
	}

    return (0);
    }
